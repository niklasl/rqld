from abc import abstractmethod
from typing import Callable, NamedTuple, Optional

from .combinators import Error, Parser, ParseResult

ParserMaker = Callable[[], Parser]


class Tagged(NamedTuple):
    name: str
    value: object


def tagged(name: str, result: ParseResult) -> ParseResult[Tagged]:
    if isinstance(result, Error):
        return result

    next_input, value = result

    return next_input, Tagged(name, value)


class TaggedParser(Parser[Tagged]):
    tag: str
    parser: Parser

    def __init__(self, tag: str, parser: Parser):
        self.tag = tag
        self.parser = parser

    def parse(self, input: str):
        return tagged(self.tag, self.parser.parse(input))


class RecursiveParser(Parser[Tagged]):
    tag: str
    parser: Parser | None
    maker: ParserMaker

    def __init__(self, tag: str, maker: ParserMaker):
        self.tag = tag
        self.maker = maker
        self.parser = None

    def parse(self, input: str):
        if self.parser is None:
            self.parser = self.maker()
        return tagged(self.tag, self.parser.parse(input))


def parser_maker(maker: ParserMaker, tag: str | None = None) -> ParserMaker:
    parser = None

    def f() -> Parser:
        nonlocal parser
        if parser is None:
            parser = maker()
            if tag:
                parser = TaggedParser(tag, parser)

        return parser

    return f


def tagged_parser(tag: str, maker: ParserMaker) -> ParserMaker:
    return parser_maker(maker, tag)


Cons = tuple[object, Optional['Cons']]


class TaggedResultTransformer:
    def transform(self, item):
        if isinstance(item, Tagged):
            return self.match(item)
        elif isinstance(item, (tuple, list)):
            return [self.transform(it) for it in item]
        else:
            return item

    def match(self, item: Tagged):
        name, value = item
        hkey = f'match_{name}'
        if hasattr(self, hkey):
            return getattr(self, hkey)(value)
        else:
            return {name: self.transform(value)}

    def _cons_to_list(self, rest: Cons | None) -> list:
        items = []
        while rest:
            first, rest = rest
            items.append(self.transform(first))
        return items

    def _wrapped_cons_to_list(self, wrapped_cons) -> list:
        items = []
        while wrapped_cons:
            first, next = wrapped_cons
            items.append(self.transform(first))
            if next is None:
                break
            wrapped_cons = next.value
        return items

    def _first_more_to_list(self, first_and_rest: tuple[object, list]) -> list:
        first, items = first_and_rest
        items.insert(0, first)
        return [self.transform(it) for it in items]
