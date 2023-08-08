from abc import abstractmethod
from typing import NamedTuple, Optional

from .combinators import Error, Parser, ParseResult


class Tagged(NamedTuple):
    name: str
    value: object


def tagged(name: str, result: ParseResult) -> ParseResult[Tagged]:
    if isinstance(result, Error):
        return result

    next_input, value = result

    return next_input, Tagged(name, value)


class TaggingParser(Parser):
    tag: str

    def __init__(self):
        self.tag = type(self).__name__

    def parse(self, input: str):
        parser = self.parser()
        return tagged(self.tag, parser.parse(input))

    @abstractmethod
    def parser(self) -> Parser:
        ...


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
