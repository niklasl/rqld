import re
from abc import abstractmethod
from functools import reduce
from typing import Callable, Generic, NamedTuple, TypeVar, cast

from mypy_extensions import trait


class Error(NamedTuple):
    on: str


Output = TypeVar('Output')

ParseResult = tuple[str, Output] | Error


class Parser(Generic[Output]):
    @abstractmethod
    def parse(self, input: str) -> ParseResult[Output]:
        ...


class MatchString(Parser[str]):
    """
    >>> parser = MatchString("Hello world!")

    >>> parser.parse("Hello world!")
    ('', 'Hello world!')

    >>> parser.parse("Hello world! Hello dream world!")
    (' Hello dream world!', 'Hello world!')

    >>> parser.parse("Goodbye world!")
    Error(on='Goodbye world!')

    >>> parser = MatchString("PREFIX")

    >>> parser.parse("PREFIX :")
    (' :', 'PREFIX')
    """

    expected: str
    __slots__ = tuple(__annotations__)

    def __init__(self, expected: str):
        self.expected = expected

    def parse(self, input: str) -> ParseResult[str]:
        if input.startswith(self.expected):
            return input[len(self.expected) :], str(self.expected)
        else:
            return Error(input)


class MatchICase(Parser[str]):
    """
    >>> parser = MatchICase("PREFIX")

    >>> parser.parse("PREFIX :")
    (' :', 'PREFIX')

    >>> parser.parse("prefix :")
    (' :', 'PREFIX')

    >>> parser.parse("Prefix :")
    (' :', 'PREFIX')

    >>> parser.parse("pReFiX :")
    (' :', 'PREFIX')
    """

    expected: str
    __slots__ = tuple(__annotations__)

    def __init__(self, expected: str):
        self.expected = expected.upper()

    def parse(self, input: str) -> ParseResult[str]:
        s = input[0 : len(self.expected)]
        if s.upper() == self.expected:
            return input[len(self.expected) :], self.expected
        else:
            return Error(input)


Output1 = TypeVar('Output1')
Output2 = TypeVar('Output2')

ParseResult1 = tuple[str, Output1] | Error
ParseResult2 = tuple[str, Output2] | Error


class Pair(Generic[Output1, Output2], Parser[tuple[Output1, Output2]]):
    """
    >>> pair = Pair(MatchString('<'), MatchString('>'))
    >>> pair.parse('<>')
    ('', ('<', '>'))
    >>> pair.parse('<a>')
    Error(on='a>')
    """

    parser1: Parser[Output1]
    parser2: Parser[Output2]
    __slots__ = tuple(__annotations__)

    def __init__(self, parser1: Parser[Output1], parser2: Parser[Output2]):
        self.parser1 = parser1
        self.parser2 = parser2

    def parse(self, input: str) -> ParseResult[tuple[Output1, Output2]]:
        result1: ParseResult1 = self.parser1.parse(input)
        if isinstance(result1, Error):
            return result1

        output1: Output1
        input2: str
        input2, output1 = result1

        result2: ParseResult2 = self.parser2.parse(input2)
        if isinstance(result2, Error):
            return result2

        output2: Output2
        next_input: str
        next_input, output2 = result2

        return next_input, (output1, output2)


@trait
class Picker(Generic[Output1, Output2]):
    pair: Pair
    __slots__ = tuple(__annotations__)

    def __init__(self, parser1: Parser[Output1], parser2: Parser[Output2]):
        self.pair = Pair(parser1, parser2)


class Left(Generic[Output1, Output2], Parser[Output1], Picker):
    """
    >>> left = Left(CollectString(OneOrMore(AnyChar(str.isalpha))), MatchString('>'))
    >>> left.parse('a>b')
    ('b', 'a')
    >>> left.parse('<>')
    Error(on='<>')
    """
    __slots__ = ()

    def parse(self, input: str) -> ParseResult[Output1]:
        result = self.pair.parse(input)
        if isinstance(result, Error):
            return result
        next_input, (left, right) = result
        return next_input, left


class Right(Generic[Output1, Output2], Parser[Output2], Picker):
    """
    >>> right = Right(MatchString('<'), CollectString(OneOrMore(AnyChar(str.isalpha))))

    >>> right.parse('<abc some="thing"/>')
    (' some="thing"/>', 'abc')

    >>> right.parse('<>')
    Error(on='>')

    >>> right.parse('abc>')
    Error(on='abc>')
    """
    __slots__ = ()

    def parse(self, input: str) -> ParseResult[Output2]:
        result = self.pair.parse(input)
        if isinstance(result, Error):
            return result
        next_input, (left, right) = result
        return next_input, right


class OneOrMore(Parser[list[Output]]):
    """
    >>> parser = OneOrMore(MatchString("xyz"))
    >>> parser.parse("xyzxyz")
    ('', ['xyz', 'xyz'])

    >>> parser.parse("yzxyzx")
    Error(on='yzxyzx')

    >>> parser.parse("")
    Error(on='')
    """

    parser: Parser
    __slots__ = tuple(__annotations__)

    def __init__(self, parser: Parser):
        self.parser = parser

    def parse(self, input: str) -> ParseResult:
        results: list = []
        next_input: str

        result = self.parser.parse(input)
        if isinstance(result, Error):
            return result

        next_input, item = result
        results.append(item)

        while not isinstance(result := self.parser.parse(next_input), Error):
            next_input, item = result
            results.append(item)

        return next_input, results


class ZeroOrMore(Parser[list[Output]]):
    """
    >>> parser = ZeroOrMore(MatchString("xyz"))
    >>> parser.parse("xyzxyz")
    ('', ['xyz', 'xyz'])

    >>> parser.parse("yzxyzx")
    ('yzxyzx', [])

    >>> parser.parse("")
    ('', [])
    """

    parser: Parser
    __slots__ = tuple(__annotations__)

    def __init__(self, parser: Parser):
        self.parser = parser

    def parse(self, input: str) -> ParseResult:
        results: list = []
        next_input: str = input

        while not isinstance(result := self.parser.parse(next_input), Error):
            next_input, item = result
            results.append(item)

        return next_input, results


class Either(Generic[Output1, Output2], Parser[Output1 | Output2]):
    """
    >>> either = Either(MatchString('prefix'), MatchString('base'))
    >>> either.parse('prefix a: <>')
    (' a: <>', 'prefix')

    >>> either.parse('base <>')
    (' <>', 'base')

    >>> either.parse('other')
    Error(on='other')
    """

    parser1: Parser[Output1]
    parser2: Parser[Output2]
    __slots__ = tuple(__annotations__)

    def __init__(self, parser1: Parser[Output1], parser2: Parser[Output2]):
        self.parser1 = parser1
        self.parser2 = parser2

    def parse(self, input: str) -> ParseResult:
        result1: ParseResult1 = self.parser1.parse(input)
        if not isinstance(result1, Error):
            return result1
        else:
            return self.parser2.parse(input)


class Optional(Parser):
    parser: Parser
    __slots__ = tuple(__annotations__)

    def __init__(self, parser: Parser):
        self.parser = parser

    def parse(self, input: str) -> ParseResult:
        result = self.parser.parse(input)
        if isinstance(result, Error):
            return input, None
        else:
            return result


class AnyChar(Parser[str]):
    """
    >>> parser = AnyChar(lambda c: c == 'x')

    >>> parser.parse("xyz")
    ('yz', 'x')

    >>> parser.parse("zyx")
    Error(on='zyx')
    """

    accept: Callable[[str], bool]
    __slots__ = tuple(__annotations__)

    def __init__(self, accept: Callable[[str], bool]):
        self.accept = accept

    def parse(self, input: str) -> ParseResult[str]:
        for c in input:
            if self.accept(c):
                return input[1:], c
            break

        return Error(input)


def RegExp(exp: str):
    rexp = re.compile(exp)
    return AnyChar(lambda c: rexp.match(c) is not None)


class CollectString(Parser[str]):
    """
    >>> collect = CollectString(ZeroOrMore(AnyChar(lambda c: c != '"')))

    >>> collect.parse("quote")
    ('', 'quote')
    """

    parser: Parser[list[str]]
    __slots__ = tuple(__annotations__)

    def __init__(self, parser: Parser[list[str]]):
        self.parser = parser

    def parse(self, input: str) -> ParseResult[str]:
        result = self.parser.parse(input)
        if isinstance(result, Error):
            return result

        next_input, chars = result

        return next_input, ''.join(chars)


class ReduceToString(Parser):
    parser: Parser
    __slots__ = tuple(__annotations__)

    def __init__(self, parser: Parser):
        self.parser = parser

    def parse(self, input: str) -> ParseResult[str]:
        result = self.parser.parse(input)
        if isinstance(result, Error):
            return result
        else:
            next_input, value = result
            chunks: list[str] = []
            _add_to_chunks(chunks, value)
            return next_input, ''.join(chunks)


def _add_to_chunks(chunks: list[str], value: object) -> None:
    if isinstance(value, str):
        chunks.append(value)
    elif isinstance(value, tuple):
        left, right = value
        _add_to_chunks(chunks, left)
        _add_to_chunks(chunks, right)
    elif value is not None:
        assert isinstance(value, list)
        for v in value:
            _add_to_chunks(chunks, v)


def space_char() -> Parser[str]:
    r"""
    >>> parser = space_char()

    >>> parser.parse(" ")
    ('', ' ')
    >>> parser.parse("\n")
    ('', '\n')
    >>> parser.parse(" x")
    ('x', ' ')

    >>> parser.parse("x ")
    Error(on='x ')
    >>> parser.parse("")
    Error(on='')
    """
    return AnyChar(lambda c: c.isspace())


def space_or_comment(lead: str|None = None):
    if lead is None:
        return space_char()
    else:
        return Either(
            space_char(),
            Pair(
                MatchString(cast(str, lead)),
                ZeroOrMore(AnyChar(lambda c: c != '\n'))
            ),
        )


def space0(lead: str|None = None) -> Parser:
    r"""
    >>> parser = space0()
    >>> parser.parse("")
    ('', [])
    >>> parser.parse("  \t\n")
    ('', [' ', ' ', '\t', '\n'])
    >>> parser.parse("  abc")
    ('abc', [' ', ' '])
    """
    return ZeroOrMore(space_or_comment(lead))


def space1(lead: str|None = None) -> Parser:
    r"""
    >>> parser = space1()
    >>> parser.parse("")
    Error(on='')
    >>> parser.parse("  ")
    ('', [' ', ' '])
    >>> parser.parse("  abc")
    ('abc', [' ', ' '])
    """
    return OneOrMore(space_or_comment(lead))


def space_wrap(parser: Parser, lead: str|None = None) -> Parser:
    """
    >>> parser = space_wrap(MatchString("attr"))

    >>> parser.parse('attr')
    ('', 'attr')

    >>> parser.parse('   attr   ')
    ('', 'attr')

    >>> parser.parse('   attr   ="value"')
    ('="value"', 'attr')

    """
    return Right(space0(lead), Left(parser, space0(lead)))


def space1_wrap(parser: Parser, lead: str|None = None) -> Parser:
    """
    >>> parser = space1_wrap(MatchString("word"))

    >>> parser.parse('word')
    Error(on='word')

    >>> parser.parse('   word   ')
    ('', 'word')

    """
    return Right(space1(lead), Left(parser, space1(lead)))


def either_of(*parsers):
    return reduce(Either, parsers)


def pattern_of(*parsers):
    return reduce(Pair, parsers)


if __name__ == '__main__':
    import doctest

    doctest.testmod()
