import re

from .partr.combinators import *
from .partr.tagging import Tagged, TaggedParser, TaggingParser


def cspace_wrap(parser: Parser) -> Parser:
    return space_wrap(parser, '#')


CSPACE0 = space0('#')

CSPACE1 = space1('#')


def cspace_pattern_of(*parsers) -> Parser:
    combos: list = list(parsers[:1]) + [Right(CSPACE0, p) for p in parsers[1:]]
    return reduce(Pair, combos)


def QueryUnit() -> Parser:
    # Query
    return Query()


class Query(TaggingParser):
    # Prologue ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery ) ValuesClause
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Prologue(),
            either_of(SelectQuery(), ConstructQuery(), DescribeQuery(), AskQuery()),
            ValuesClause(),
        )


def UpdateUnit() -> Parser:
    # Update
    return Update()


class Prologue(TaggingParser):
    # ( BaseDecl | PrefixDecl )*
    def parser(self) -> Parser:
        return ZeroOrMore(cspace_wrap(Either(BaseDecl(), PrefixDecl())))


class BaseDecl(TaggingParser):
    # 'BASE' IRIREF
    def parser(self) -> Parser:
        return Right(Pair(MatchICase(r'BASE'), CSPACE1), IRIREF)


class PrefixDecl(TaggingParser):
    # 'PREFIX' PNAME_NS IRIREF
    def parser(self) -> Parser:
        return Right(
            Pair(MatchICase(r'PREFIX'), CSPACE1),
            Pair(Left(PNAME_NS, CSPACE1), IRIREF),
        )


class SelectQuery(TaggingParser):
    # SelectClause DatasetClause* WhereClause SolutionModifier
    def parser(self) -> Parser:
        return cspace_pattern_of(
            SelectClause(),
            ZeroOrMore(DatasetClause()),
            WhereClause(),
            SolutionModifier(),
        )


class SubSelect(TaggingParser):
    # SelectClause WhereClause SolutionModifier ValuesClause
    def parser(self) -> Parser:
        return cspace_pattern_of(
            SelectClause(), WhereClause(), SolutionModifier(), ValuesClause()
        )


class SelectClause(TaggingParser):
    # 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Right(
                Pair(MatchICase(r"SELECT"), CSPACE1),
                Optional(Either(MatchICase(r"DISTINCT"), MatchICase(r"REDUCED"))),
            ),
            Either(
                OneOrMore(
                    Left(
                        Either(
                            Var(),
                            Right(
                                cspace_wrap(MatchString(r"(")),
                                cspace_pattern_of(
                                    Expression(),
                                    Right(
                                        cspace_wrap(MatchICase(r"AS")),
                                        Left(
                                            Var(),
                                            cspace_wrap(MatchString(r")")),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        CSPACE0,
                    )
                ),
                MatchString(r"*"),
            ),
        )


class ConstructQuery(TaggingParser):
    # 'CONSTRUCT' ( ConstructTemplate DatasetClause* WhereClause SolutionModifier | DatasetClause* 'WHERE' '{' TriplesTemplate? '}' SolutionModifier )
    def parser(self) -> Parser:
        return Right(
            Pair(MatchICase(r'CONSTRUCT'), CSPACE0),
            Either(
                cspace_pattern_of(
                    ConstructTemplate(),
                    ZeroOrMore(DatasetClause()),
                    WhereClause(),
                    SolutionModifier(),
                ),
                cspace_pattern_of(
                    ZeroOrMore(DatasetClause()),
                    Right(
                        cspace_pattern_of(
                            MatchICase(r'WHERE'), cspace_wrap(MatchString(r'{'))
                        ),
                        Left(
                            Optional(TriplesTemplate()),
                            cspace_wrap(MatchString(r'}')),
                        ),
                    ),
                    SolutionModifier(),
                ),
            ),
        )


class DescribeQuery(TaggingParser):
    # 'DESCRIBE' ( VarOrIri+ | '*' ) DatasetClause* WhereClause? SolutionModifier
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'DESCRIBE'),
            either_of(OneOrMore(VarOrIri()), MatchString(r'*')),
            ZeroOrMore(DatasetClause()),
            Optional(WhereClause()),
            SolutionModifier(),
        )


class AskQuery(TaggingParser):
    # 'ASK' DatasetClause* WhereClause SolutionModifier
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'ASK'),
            ZeroOrMore(DatasetClause()),
            WhereClause(),
            SolutionModifier(),
        )


class DatasetClause(TaggingParser):
    # 'FROM' ( DefaultGraphClause | NamedGraphClause )
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'FROM'), either_of(DefaultGraphClause(), NamedGraphClause())
        )


class DefaultGraphClause(TaggingParser):
    # SourceSelector
    def parser(self) -> Parser:
        return SourceSelector()


class NamedGraphClause(TaggingParser):
    # 'NAMED' SourceSelector
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchICase(r'NAMED'), SourceSelector())


class SourceSelector(TaggingParser):
    # iri
    def parser(self) -> Parser:
        return iri()


def WhereClause() -> Parser:
    # 'WHERE'? GroupGraphPattern
    return Right((Optional(Pair(MatchICase(r'WHERE'), CSPACE0))), GroupGraphPattern())


class SolutionModifier(TaggingParser):
    # GroupClause? HavingClause? OrderClause? LimitOffsetClauses?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Optional(GroupClause()),
            Optional(HavingClause()),
            Optional(OrderClause()),
            Optional(LimitOffsetClauses()),
        )


def GroupClause() -> Parser:
    # 'GROUP' 'BY' GroupCondition+
    return Right(
        cspace_wrap(cspace_pattern_of(MatchICase(r'GROUP'), MatchICase(r'BY'))),
        OneOrMore(cspace_wrap(GroupCondition())),
    )


class GroupCondition(TaggingParser):
    # BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var
    def parser(self) -> Parser:
        return either_of(
            BuiltInCall(),
            FunctionCall(),
            Right(
                cspace_wrap(MatchString(r'(')),
                Left(
                    cspace_pattern_of(
                        Expression(),
                        Optional(Right(cspace_wrap(MatchICase(r'AS')), Var())),
                    ),
                    cspace_wrap(MatchString(r')')),
                ),
            ),
            Var(),
        )


def HavingClause() -> Parser:
    # 'HAVING' HavingCondition+
    return Right(MatchICase(r'HAVING'), OneOrMore(cspace_wrap(HavingCondition())))


def HavingCondition() -> Parser:
    # Constraint
    return Constraint()


def OrderClause() -> Parser:
    # 'ORDER' 'BY' OrderCondition+
    return Right(
        cspace_wrap(cspace_pattern_of(MatchICase(r'ORDER'), MatchICase(r'BY'))),
        OneOrMore(cspace_wrap(OrderCondition())),
    )


class OrderCondition(TaggingParser):
    # ( ( 'ASC' | 'DESC' ) BrackettedExpression ) | ( Constraint | Var )
    def parser(self) -> Parser:
        return Either(
            cspace_pattern_of(
                Either(MatchICase(r'ASC'), MatchICase(r'DESC')), BrackettedExpression()
            ),
            Either(Constraint(), Var()),
        )


def LimitOffsetClauses() -> Parser:
    # LimitClause OffsetClause? | OffsetClause LimitClause?
    return Either(
        cspace_pattern_of(LimitClause(), Optional(OffsetClause())),
        cspace_pattern_of(OffsetClause(), Optional(LimitClause())),
    )


class LimitClause(TaggingParser):
    # 'LIMIT' INTEGER
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'LIMIT')), INTEGER)


class OffsetClause(TaggingParser):
    # 'OFFSET' INTEGER
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'OFFSET')), INTEGER)


def ValuesClause() -> Parser:
    # ( 'VALUES' DataBlock )?
    return Optional(Right(cspace_wrap(MatchICase(r'VALUES')), DataBlock()))


class Update(TaggingParser):
    # Prologue ( Update1 ( ';' Update )? )?
    def parser(self) -> Parser:
        return pattern_of(
            Prologue(),
            Optional(
                cspace_pattern_of(
                    Update1(), Optional(Right(cspace_wrap(MatchString(r';')), Update()))
                )
            ),
        )


def Update1() -> Parser:
    # Load | Clear | Drop | Add | Move | Copy | Create | InsertData | DeleteData | DeleteWhere | Modify
    return either_of(
        Load(),
        Clear(),
        Drop(),
        Add(),
        Move(),
        Copy(),
        Create(),
        InsertData(),
        DeleteData(),
        DeleteWhere(),
        Modify(),
    )


class Load(TaggingParser):
    # 'LOAD' 'SILENT'? iri ( 'INTO' GraphRef )?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'LOAD'),
            Optional(MatchICase(r'SILENT')),
            iri(),
            Optional(cspace_pattern_of(MatchICase(r'INTO'), GraphRef())),
        )


class Clear(TaggingParser):
    # 'CLEAR' 'SILENT'? GraphRefAll
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'CLEAR'), Optional(MatchICase(r'SILENT')), GraphRefAll()
        )


class Drop(TaggingParser):
    # 'DROP' 'SILENT'? GraphRefAll
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'DROP'), Optional(MatchICase(r'SILENT')), GraphRefAll()
        )


class Create(TaggingParser):
    # 'CREATE' 'SILENT'? GraphRef
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'CREATE'), Optional(MatchICase(r'SILENT')), GraphRef()
        )


class Add(TaggingParser):
    # 'ADD' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'ADD'),
            Optional(MatchICase(r'SILENT')),
            GraphOrDefault(),
            MatchICase(r'TO'),
            GraphOrDefault(),
        )


class Move(TaggingParser):
    # 'MOVE' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'MOVE'),
            Optional(MatchICase(r'SILENT')),
            GraphOrDefault(),
            MatchICase(r'TO'),
            GraphOrDefault(),
        )


class Copy(TaggingParser):
    # 'COPY' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MatchICase(r'COPY'),
            Optional(MatchICase(r'SILENT')),
            GraphOrDefault(),
            MatchICase(r'TO'),
            GraphOrDefault(),
        )


class InsertData(TaggingParser):
    # 'INSERT DATA' QuadData
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchString(r'INSERT DATA'), QuadData())


class DeleteData(TaggingParser):
    # 'DELETE DATA' QuadData
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchString(r'DELETE DATA'), QuadData())


class DeleteWhere(TaggingParser):
    # 'DELETE WHERE' QuadPattern
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchString(r'DELETE WHERE'), QuadPattern())


class Modify(TaggingParser):
    # ( 'WITH' iri )? ( DeleteClause InsertClause? | InsertClause ) UsingClause* 'WHERE' GroupGraphPattern
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Optional(Right(cspace_wrap(MatchICase(r"WITH")), iri())),
            Either(
                pattern_of(DeleteClause(), Optional(InsertClause())),
                InsertClause(),
            ),
            ZeroOrMore(UsingClause()),
            Right(cspace_wrap(MatchICase(r"WHERE")), GroupGraphPattern()),
        )


class DeleteClause(TaggingParser):
    # 'DELETE' QuadPattern
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'DELETE')), QuadPattern())


class InsertClause(TaggingParser):
    # 'INSERT' QuadPattern
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'INSERT')), QuadPattern())


class UsingClause(TaggingParser):
    # 'USING' ( iri | 'NAMED' iri )
    def parser(self) -> Parser:
        return Right(
            cspace_wrap(MatchICase(r'USING')),
            Either(iri(), Right(cspace_wrap(MatchICase(r'NAMED')), iri())),
        )


class GraphOrDefault(TaggingParser):
    # 'DEFAULT' | 'GRAPH'? iri
    def parser(self) -> Parser:
        return Either(
            MatchICase(r'DEFAULT'),
            cspace_pattern_of(Optional(MatchICase(r'GRAPH')), iri()),
        )


class GraphRef(TaggingParser):
    # 'GRAPH' iri
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchICase(r'GRAPH'), iri())


class GraphRefAll(TaggingParser):
    # GraphRef | 'DEFAULT' | 'NAMED' | 'ALL'
    def parser(self) -> Parser:
        return either_of(
            GraphRef(), MatchICase(r'DEFAULT'), MatchICase(r'NAMED'), MatchICase(r'ALL')
        )


def QuadPattern() -> Parser:
    # '{' Quads '}'
    return Right(
        cspace_wrap(MatchString(r'{')), Left(Quads(), cspace_wrap(MatchString(r'}')))
    )


def QuadData() -> Parser:
    # '{' Quads '}'
    return QuadPattern()


class Quads(TaggingParser):
    # TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Optional(TriplesTemplate()),
            ZeroOrMore(
                cspace_pattern_of(
                    Left(QuadsNotTriples(), Optional(cspace_wrap(MatchString(r".")))),
                    Optional(TriplesTemplate()),
                )
            ),
        )


class QuadsNotTriples(TaggingParser):
    # 'GRAPH' VarOrIri '{' TriplesTemplate? '}'
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Right(cspace_wrap(MatchICase(r"GRAPH")), VarOrIri()),
            Right(
                cspace_wrap(MatchString(r"{")),
                Left(Optional(TriplesTemplate()), cspace_wrap(MatchString(r"}"))),
            ),
        )


class TriplesTemplate(TaggingParser):
    # TriplesSameSubject ( '.' TriplesTemplate? )?
    def parser(self) -> Parser:
        return Pair(
            TriplesSameSubject(),
            Optional(
                Right(cspace_wrap(MatchString(r'.')), Optional(TriplesTemplate()))
            ),
        )


class GroupGraphPattern(TaggingParser):
    # '{' ( SubSelect | GroupGraphPatternSub ) '}'
    def parser(self) -> Parser:
        return Right(
            Pair(MatchString(r'{'), CSPACE0),
            Left(
                Either(SubSelect(), GroupGraphPatternSub()),
                Pair(CSPACE0, MatchString(r'}')),
            ),
        )


class GroupGraphPatternSub(TaggingParser):
    # TriplesBlock? ( GraphPatternNotTriples '.'? TriplesBlock? )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Optional(TriplesBlock()),
            ZeroOrMore(
                cspace_pattern_of(
                    Left(
                        GraphPatternNotTriples(),
                        cspace_wrap(Optional(MatchString(r'.'))),
                    ),
                    Optional(TriplesBlock()),
                )
            ),
        )


class TriplesBlock(TaggingParser):
    # TriplesSameSubjectPath ( '.' TriplesBlock? )?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            TriplesSameSubjectPath(),
            Optional(Right(cspace_wrap(MatchString(r'.')), Optional(TriplesBlock()))),
        )


def GraphPatternNotTriples() -> Parser:
    # GroupOrUnionGraphPattern | OptionalGraphPattern | MinusGraphPattern | GraphGraphPattern | ServiceGraphPattern | Filter | Bind | InlineData
    return either_of(
        GroupOrUnionGraphPattern(),
        OptionalGraphPattern(),
        MinusGraphPattern(),
        GraphGraphPattern(),
        ServiceGraphPattern(),
        Filter(),
        Bind(),
        InlineData(),
    )


class OptionalGraphPattern(TaggingParser):
    # 'OPTIONAL' GroupGraphPattern
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'OPTIONAL')), GroupGraphPattern())


class GraphGraphPattern(TaggingParser):
    # 'GRAPH' VarOrIri GroupGraphPattern
    def parser(self) -> Parser:
        return Right(
            cspace_wrap(MatchICase(r'GRAPH')),
            cspace_pattern_of(VarOrIri(), GroupGraphPattern()),
        )


class ServiceGraphPattern(TaggingParser):
    # 'SERVICE' 'SILENT'? VarOrIri GroupGraphPattern
    def parser(self) -> Parser:
        return Right(
            cspace_wrap(MatchICase(r'SERVICE')),
            cspace_pattern_of(
                Optional(MatchICase(r'SILENT')),
                VarOrIri(),
                GroupGraphPattern(),
            ),
        )


class Bind(TaggingParser):
    # 'BIND' '(' Expression 'AS' Var ')'
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Right(
                cspace_pattern_of(MatchICase(r"BIND"), MatchString(r"(")),
                Expression(),
            ),
            Left(
                Right(cspace_wrap(MatchICase(r"AS")), Var()),
                cspace_wrap(MatchString(r")")),
            ),
        )


class InlineData(TaggingParser):
    # 'VALUES' DataBlock
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'VALUES')), DataBlock())


def DataBlock() -> Parser:
    # InlineDataOneVar | InlineDataFull
    return Either(InlineDataOneVar(), InlineDataFull())


class InlineDataOneVar(TaggingParser):
    # Var '{' DataBlockValue* '}'
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Var(),
            Right(
                cspace_wrap(MatchString(r'{')),
                Left(
                    ZeroOrMore(cspace_wrap(DataBlockValue())),
                    cspace_wrap(MatchString(r'}')),
                ),
            ),
        )


class InlineDataFull(TaggingParser):
    # ( NIL | '(' Var* ')' ) '{' ( '(' DataBlockValue* ')' | NIL )* '}'
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Either(
                NIL,
                Right(
                    cspace_wrap(MatchString(r'(')),
                    Left(
                        ZeroOrMore(Left(Var(), CSPACE0)),
                        cspace_wrap(MatchString(r')')),
                    ),
                ),
            ),
            Right(
                cspace_wrap(MatchString(r'{')),
                Left(
                    ZeroOrMore(
                        cspace_wrap(
                            Either(
                                Right(
                                    cspace_wrap(MatchString(r'(')),
                                    Left(
                                        ZeroOrMore(Left(DataBlockValue(), CSPACE0)),
                                        cspace_wrap(MatchString(r')')),
                                    ),
                                ),
                                NIL,
                            )
                        )
                    ),
                    cspace_wrap(MatchString(r'}')),
                ),
            ),
        )


def DataBlockValue() -> Parser:
    # iri | RDFLiteral | NumericLiteral | BooleanLiteral | 'UNDEF'
    return either_of(
        iri(), RDFLiteral(), NumericLiteral(), BooleanLiteral(), MatchICase(r'UNDEF')
    )


class MinusGraphPattern(TaggingParser):
    # 'MINUS' GroupGraphPattern
    def parser(self) -> Parser:
        return cspace_pattern_of(MatchICase(r'MINUS'), GroupGraphPattern())


class GroupOrUnionGraphPattern(TaggingParser):
    # GroupGraphPattern ( 'UNION' GroupGraphPattern )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            GroupGraphPattern(),
            ZeroOrMore(Right(cspace_wrap(MatchICase(r"UNION")), GroupGraphPattern())),
        )


class Filter(TaggingParser):
    # 'FILTER' Constraint
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'FILTER')), Constraint())


def Constraint() -> Parser:
    # BrackettedExpression | BuiltInCall | FunctionCall
    return either_of(BrackettedExpression(), BuiltInCall(), FunctionCall())


class FunctionCall(TaggingParser):
    # iri ArgList
    def parser(self) -> Parser:
        return cspace_pattern_of(iri(), ArgList())


class ArgList(TaggingParser):
    # NIL | '(' 'DISTINCT'? Expression ( ',' Expression )* ')'
    def parser(self) -> Parser:
        return Either(
            NIL,
            cspace_pattern_of(
                MatchString(r'('),
                Optional(MatchICase(r'DISTINCT')),
                Expression(),
                ZeroOrMore(cspace_pattern_of(MatchString(r','), Expression())),
                MatchString(r')'),
            ),
        )


class ExpressionList(TaggingParser):
    # NIL | '(' Expression ( ',' Expression )* ')'
    def parser(self) -> Parser:
        return Either(
            NIL,
            Pair(
                Right(cspace_wrap(MatchString(r"(")), Expression()),
                Left(
                    ZeroOrMore(Right(cspace_wrap(MatchString(r",")), Expression())),
                    cspace_wrap(MatchString(r")")),
                ),
            ),
        )


def ConstructTemplate() -> Parser:
    # '{' ConstructTriples? '}'
    return Right(
        Pair(MatchString(r'{'), CSPACE0),
        Left(Optional(ConstructTriples()), Pair(CSPACE0, MatchString(r'}'))),
    )


class ConstructTriples(TaggingParser):
    # TriplesSameSubject ( '.' ConstructTriples? )?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            TriplesSameSubject(),
            Optional(Right(MatchString(r'.'), Optional(ConstructTriples()))),
        )


class TriplesSameSubject(TaggingParser):
    """
    >>> TriplesSameSubject().parse('<abc> :def ?ghi')
    ('', Tagged(name='TriplesSameSubject', value=(Tagged(name='IRIREF', value='abc'), Tagged(name='PropertyListNotEmpty', value=((Tagged(name='Verb', value=Tagged(name='PrefixedName', value=(':', 'def'))), Tagged(name='ObjectList', value=(Tagged(name='VAR1', value='ghi'), []))), [])))))
    """

    # VarOrTerm PropertyListNotEmpty | TriplesNode PropertyList
    def parser(self) -> Parser:
        return either_of(
            cspace_pattern_of(VarOrTerm(), PropertyListNotEmpty()),
            cspace_pattern_of(TriplesNode(), PropertyList()),
        )


class PropertyList(TaggingParser):
    # PropertyListNotEmpty?
    def parser(self) -> Parser:
        return Optional(PropertyListNotEmpty())


class PropertyListNotEmpty(TaggingParser):
    """
    >>> PropertyListNotEmpty().parse(':abc ?def')
    ('', Tagged(name='PropertyListNotEmpty', value=((Tagged(name='Verb', value=Tagged(name='PrefixedName', value=(':', 'abc'))), Tagged(name='ObjectList', value=(Tagged(name='VAR1', value='def'), []))), [])))
    """

    # Verb ObjectList ( ';' ( Verb ObjectList )? )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Verb(),
            ObjectList(),
            ZeroOrMore(
                Right(
                    Pair(MatchString(r';'), CSPACE0),
                    Optional(cspace_pattern_of(Verb(), ObjectList())),
                )
            ),
        )


class Verb(TaggingParser):
    """
    >>> Verb().parse('?var')
    ('', Tagged(name='Verb', value=Tagged(name='VAR1', value='var')))
    >>> Verb().parse('a')
    ('', Tagged(name='Verb', value='a'))
    """

    # VarOrIri | 'a'
    def parser(self) -> Parser:
        return either_of(VarOrIri(), MatchString(r'a'))


class ObjectList(TaggingParser):
    """
    >>> ObjectList().parse('<abc>, <def>')
    ('', Tagged(name='ObjectList', value=(Tagged(name='IRIREF', value='abc'), [Tagged(name='IRIREF', value='def')])))

    >>> ObjectList().parse('?def')
    ('', Tagged(name='ObjectList', value=(Tagged(name='VAR1', value='def'), [])))

    >>> ObjectList().parse('<abc>, ?def')
    ('', Tagged(name='ObjectList', value=(Tagged(name='IRIREF', value='abc'), [Tagged(name='VAR1', value='def')])))
    """

    # Object ( ',' Object )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Object(), ZeroOrMore(Right(cspace_wrap(MatchString(r',')), Object()))
        )


def Object() -> Parser:
    """
    >>> Object().parse('<abc>')
    ('', Tagged(name='IRIREF', value='abc'))

    >>> Object().parse('?abc')
    ('', Tagged(name='VAR1', value='abc'))
    """
    # GraphNode
    return GraphNode()


class TriplesSameSubjectPath(TaggingParser):
    # VarOrTerm PropertyListPathNotEmpty | TriplesNodePath PropertyListPath
    def parser(self) -> Parser:
        return Either(
            cspace_pattern_of(VarOrTerm(), PropertyListPathNotEmpty()),
            cspace_pattern_of(TriplesNodePath(), PropertyListPath()),
        )


class PropertyListPath(TaggingParser):
    # PropertyListPathNotEmpty?
    def parser(self) -> Parser:
        return Optional(PropertyListPathNotEmpty())


class PropertyListPathNotEmpty(TaggingParser):
    # ( VerbPath | VerbSimple ) ObjectListPath ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            Either(VerbPath(), VerbSimple()),
            ObjectListPath(),
            ZeroOrMore(
                Right(
                    cspace_wrap(MatchString(r';')),
                    Optional(
                        cspace_pattern_of(
                            either_of(VerbPath(), VerbSimple()), ObjectList()
                        )
                    ),
                )
            ),
        )


def VerbPath() -> Parser:
    # Path
    return Path()


def VerbSimple() -> Parser:
    # Var
    return Var()


class ObjectListPath(TaggingParser):
    # ObjectPath ( ',' ObjectPath )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            ObjectPath(),
            ZeroOrMore(Right(cspace_wrap(MatchString(r',')), ObjectPath())),
        )


def ObjectPath() -> Parser:
    # GraphNodePath
    return GraphNodePath()


def Path() -> Parser:
    # PathAlternative
    return PathAlternative()


class PathAlternative(TaggingParser):
    # PathSequence ( '|' PathSequence )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            PathSequence(),
            ZeroOrMore(Right(cspace_wrap(MatchString(r'|')), PathSequence())),
        )


class PathSequence(TaggingParser):
    # PathEltOrInverse ( '/' PathEltOrInverse )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            PathEltOrInverse(),
            ZeroOrMore(Right(cspace_wrap(MatchString(r'/')), PathEltOrInverse())),
        )


class PathElt(TaggingParser):
    # PathPrimary PathMod?
    def parser(self) -> Parser:
        return Pair(PathPrimary(), Optional(PathMod()))


class PathEltOrInverse(TaggingParser):
    # PathElt | '^' PathElt
    def parser(self) -> Parser:
        return Either(PathElt(), cspace_pattern_of(MatchString(r'^'), PathElt()))


def PathMod() -> Parser:
    # '?' | '*' | '+'
    return either_of(MatchString(r'?'), MatchString(r'*'), MatchString(r'+'))


class PathPrimary(TaggingParser):
    # iri | 'a' | '!' PathNegatedPropertySet | '(' Path ')'
    def parser(self) -> Parser:
        return either_of(
            iri(),
            MatchString(r'a'),
            Right(Pair(MatchString(r'!'), CSPACE0), PathNegatedPropertySet()),
            Left(
                Right(Pair(MatchString(r'('), CSPACE0), Path()),
                Pair(CSPACE0, MatchString(r')')),
            ),
        )


class PathNegatedPropertySet(TaggingParser):
    # PathOneInPropertySet | '(' ( PathOneInPropertySet ( '|' PathOneInPropertySet )* )? ')'
    def parser(self) -> Parser:
        return Either(
            PathOneInPropertySet(),
            cspace_pattern_of(
                MatchString(r'('),
                Optional(
                    cspace_pattern_of(
                        PathOneInPropertySet(),
                        ZeroOrMore(
                            cspace_pattern_of(MatchString(r'|'), PathOneInPropertySet())
                        ),
                    )
                ),
                MatchString(r')'),
            ),
        )


class PathOneInPropertySet(TaggingParser):
    # iri | 'a' | '^' ( iri | 'a' )
    def parser(self) -> Parser:
        return either_of(
            iri(),
            MatchString(r'a'),
            cspace_pattern_of(MatchString(r'^'), either_of(iri(), MatchString(r'a'))),
        )


class Integer(TaggingParser):
    # INTEGER
    def parser(self) -> Parser:
        return INTEGER


def TriplesNode() -> Parser:
    # Collection | BlankNodePropertyList
    return Either(Collection(), BlankNodePropertyList())


class BlankNodePropertyList(TaggingParser):
    # '[' PropertyListNotEmpty ']'
    def parser(self) -> Parser:
        return Left(
            Right(cspace_wrap(MatchString(r'[')), PropertyListNotEmpty()),
            MatchString(r']'),
        )


def TriplesNodePath() -> Parser:
    # CollectionPath | BlankNodePropertyListPath
    return Either(CollectionPath(), BlankNodePropertyListPath())


class BlankNodePropertyListPath(TaggingParser):
    # '[' PropertyListPathNotEmpty ']'
    def parser(self) -> Parser:
        return Right(
            cspace_wrap(MatchString(r'[')),
            Left(
                PropertyListPathNotEmpty(),
                cspace_wrap(MatchString(r']')),
            ),
        )


class Collection(TaggingParser):
    """
    >>> Collection().parse('(<abc>)')
    ('', Tagged(name='Collection', value=[Tagged(name='IRIREF', value='abc')]))
    """

    # '(' GraphNode+ ')'
    def parser(self) -> Parser:
        return Right(
            MatchString(r'('),
            Left(OneOrMore(cspace_wrap(GraphNode())), MatchString(r')')),
        )


class CollectionPath(TaggingParser):
    # '(' GraphNodePath+ ')'
    def parser(self) -> Parser:
        return Right(
            MatchString(r'('),
            Left(OneOrMore(cspace_wrap(GraphNodePath())), MatchString(r')')),
        )


def GraphNode() -> Parser:
    """
    >>> GraphNode().parse('<abc>')
    ('', Tagged(name='IRIREF', value='abc'))
    """
    # VarOrTerm | TriplesNode
    return either_of(VarOrTerm(), TriplesNode())


def GraphNodePath() -> Parser:
    # VarOrTerm | TriplesNodePath
    return either_of(VarOrTerm(), TriplesNodePath())


def VarOrTerm() -> Parser:
    # Var | GraphTerm
    return either_of(Var(), GraphTerm())


def VarOrIri() -> Parser:
    """
    >>> VarOrIri().parse('?var')
    ('', Tagged(name='VAR1', value='var'))
    """
    # Var | iri
    return Either(Var(), iri())


def Var() -> Parser:
    # VAR1 | VAR2
    return Either(VAR1, VAR2)


def GraphTerm() -> Parser:
    # iri | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
    return either_of(
        iri(), RDFLiteral(), NumericLiteral(), BooleanLiteral(), BlankNode(), NIL
    )


def Expression() -> Parser:
    # ConditionalOrExpression
    return ConditionalOrExpression()


class ConditionalOrExpression(TaggingParser):
    # ConditionalAndExpression ( '||' ConditionalAndExpression )*
    def parser(self) -> Parser:
        return Pair(
            Left(ConditionalAndExpression(), CSPACE0),
            ZeroOrMore(
                Right(cspace_wrap(MatchString(r'||')), ConditionalAndExpression())
            ),
        )


class ConditionalAndExpression(TaggingParser):
    # ValueLogical ( '&&' ValueLogical )*
    def parser(self) -> Parser:
        return Pair(
            Left(ValueLogical(), CSPACE0),
            ZeroOrMore(Right(cspace_wrap(MatchString(r'&&')), ValueLogical())),
        )


def ValueLogical() -> Parser:
    # RelationalExpression
    return RelationalExpression()


class RelationalExpression(TaggingParser):
    # NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression | 'IN' ExpressionList | 'NOT' 'IN' ExpressionList )?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            NumericExpression(),
            Optional(
                either_of(
                    cspace_pattern_of(MatchString(r'='), NumericExpression()),
                    cspace_pattern_of(MatchString(r'!='), NumericExpression()),
                    cspace_pattern_of(MatchString(r'<'), NumericExpression()),
                    cspace_pattern_of(MatchString(r'>'), NumericExpression()),
                    cspace_pattern_of(MatchString(r'<='), NumericExpression()),
                    cspace_pattern_of(MatchString(r'>='), NumericExpression()),
                    cspace_pattern_of(MatchICase(r'IN'), ExpressionList()),
                    cspace_pattern_of(
                        MatchICase(r'NOT'),
                        MatchICase(r'IN'),
                        ExpressionList(),
                    ),
                )
            ),
        )


def NumericExpression() -> Parser:
    # AdditiveExpression
    return AdditiveExpression()


class AdditiveExpression(TaggingParser):
    # MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | ( NumericLiteralPositive | NumericLiteralNegative ) ( ( '*' UnaryExpression ) | ( '/' UnaryExpression ) )* )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            MultiplicativeExpression(),
            ZeroOrMore(
                either_of(
                    cspace_pattern_of(MatchString(r'+'), MultiplicativeExpression()),
                    cspace_pattern_of(MatchString(r'-'), MultiplicativeExpression()),
                    cspace_pattern_of(
                        Either(NumericLiteralPositive(), NumericLiteralNegative()),
                        ZeroOrMore(
                            either_of(
                                cspace_pattern_of(MatchString(r'*'), UnaryExpression()),
                                cspace_pattern_of(MatchString(r'/'), UnaryExpression()),
                            )
                        ),
                    ),
                )
            ),
        )


class MultiplicativeExpression(TaggingParser):
    # UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
    def parser(self) -> Parser:
        return cspace_pattern_of(
            UnaryExpression(),
            ZeroOrMore(
                Either(
                    cspace_pattern_of(MatchString(r'*'), UnaryExpression()),
                    cspace_pattern_of(MatchString(r'/'), UnaryExpression()),
                )
            ),
        )


class UnaryExpression(TaggingParser):
    # '!' PrimaryExpression | '+' PrimaryExpression | '-' PrimaryExpression | PrimaryExpression
    def parser(self) -> Parser:
        return either_of(
            cspace_pattern_of(MatchString(r'!'), PrimaryExpression()),
            cspace_pattern_of(MatchString(r'+'), PrimaryExpression()),
            cspace_pattern_of(MatchString(r'-'), PrimaryExpression()),
            PrimaryExpression(),
        )


def PrimaryExpression() -> Parser:
    # BrackettedExpression | BuiltInCall | iriOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
    return either_of(
        BrackettedExpression(),
        BuiltInCall(),
        iriOrFunction(),
        RDFLiteral(),
        NumericLiteral(),
        BooleanLiteral(),
        Var(),
    )


def BrackettedExpression() -> Parser:
    # '(' Expression ')'
    return Right(
        Pair(MatchString(r'('), CSPACE0),
        Left(Expression(), Pair(CSPACE0, MatchString(r')'))),
    )


def _parse_call1_with(fname, expr):
    return pattern_of(
        Left(MatchICase(fname), cspace_wrap(MatchString(r'('))),
        Left(expr, cspace_wrap(MatchString(r')'))),
    )


def _parse_call1(fname):
    return _parse_call1_with(fname, Expression())


def _parse_call2(fname):
    return pattern_of(
        Left(MatchICase(fname), cspace_wrap(MatchString(r'('))),
        Left(Expression(), cspace_wrap(MatchString(r','))),
        Left(Expression(), cspace_wrap(MatchString(r')'))),
    )


class BuiltInCall(TaggingParser):
    # Aggregate | 'STR' '(' Expression ')' | 'LANG' '(' Expression ')' | 'LANGMATCHES' '(' Expression ',' Expression ')' | 'DATATYPE' '(' Expression ')' | 'BOUND' '(' Var ')' | 'IRI' '(' Expression ')' | 'URI' '(' Expression ')' | 'BNODE' ( '(' Expression ')' | NIL ) | 'RAND' NIL | 'ABS' '(' Expression ')' | 'CEIL' '(' Expression ')' | 'FLOOR' '(' Expression ')' | 'ROUND' '(' Expression ')' | 'CONCAT' ExpressionList | SubstringExpression | 'STRLEN' '(' Expression ')' | StrReplaceExpression | 'UCASE' '(' Expression ')' | 'LCASE' '(' Expression ')' | 'ENCODE_FOR_URI' '(' Expression ')' | 'CONTAINS' '(' Expression ',' Expression ')' | 'STRSTARTS' '(' Expression ',' Expression ')' | 'STRENDS' '(' Expression ',' Expression ')' | 'STRBEFORE' '(' Expression ',' Expression ')' | 'STRAFTER' '(' Expression ',' Expression ')' | 'YEAR' '(' Expression ')' | 'MONTH' '(' Expression ')' | 'DAY' '(' Expression ')' | 'HOURS' '(' Expression ')' | 'MINUTES' '(' Expression ')' | 'SECONDS' '(' Expression ')' | 'TIMEZONE' '(' Expression ')' | 'TZ' '(' Expression ')' | 'NOW' NIL | 'UUID' NIL | 'STRUUID' NIL | 'MD5' '(' Expression ')' | 'SHA1' '(' Expression ')' | 'SHA256' '(' Expression ')' | 'SHA384' '(' Expression ')' | 'SHA512' '(' Expression ')' | 'COALESCE' ExpressionList | 'IF' '(' Expression ',' Expression ',' Expression ')' | 'STRLANG' '(' Expression ',' Expression ')' | 'STRDT' '(' Expression ',' Expression ')' | 'sameTerm' '(' Expression ',' Expression ')' | 'isIRI' '(' Expression ')' | 'isURI' '(' Expression ')' | 'isBLANK' '(' Expression ')' | 'isLITERAL' '(' Expression ')' | 'isNUMERIC' '(' Expression ')' | RegexExpression | ExistsFunc | NotExistsFunc
    def parser(self) -> Parser:
        return either_of(
            Aggregate(),
            _parse_call1('STR'),
            _parse_call1('LANG'),
            _parse_call2('LANGMATCHES'),
            _parse_call1('DATATYPE'),
            _parse_call1_with('BOUND', Var()),
            _parse_call1('IRI'),
            _parse_call1('URI'),
            cspace_pattern_of(
                MatchICase('BNODE'),
                Either(
                    Right(
                        cspace_wrap(MatchString(r'(')),
                        Left(Expression(), cspace_wrap(MatchString(r')'))),
                    ),
                    NIL,
                ),
            ),
            cspace_pattern_of(MatchICase(r'RAND'), NIL),
            _parse_call1('ABS'),
            _parse_call1('CEIL'),
            _parse_call1('FLOOR'),
            _parse_call1('ROUND'),
            cspace_pattern_of(MatchICase(r'CONCAT'), ExpressionList()),
            SubstringExpression(),
            _parse_call1('STRLEN'),
            StrReplaceExpression(),
            _parse_call1('UCASE'),
            _parse_call1('LCASE'),
            _parse_call1('ENCODE_FOR_URI'),
            _parse_call2('CONTAINS'),
            _parse_call2('STRSTARTS'),
            _parse_call2('STRENDS'),
            _parse_call2('STRBEFORE'),
            _parse_call2('STRAFTER'),
            _parse_call1('YEAR'),
            _parse_call1('MONTH'),
            _parse_call1('DAY'),
            _parse_call1('HOURS'),
            _parse_call1('MINUTES'),
            _parse_call1('SECONDS'),
            _parse_call1('TIMEZONE'),
            _parse_call1('TZ'),
            pattern_of(MatchICase(r'NOW'), Right(CSPACE0, NIL)),
            pattern_of(MatchICase(r'UUID'), Right(CSPACE0, NIL)),
            pattern_of(MatchICase(r'STRUUID'), Right(CSPACE0, NIL)),
            _parse_call1('MD5'),
            _parse_call1('SHA1'),
            _parse_call1('SHA256'),
            _parse_call1('SHA384'),
            _parse_call1('SHA512'),
            cspace_pattern_of(MatchICase(r'COALESCE'), ExpressionList()),
            pattern_of(
                Left(MatchICase('IF'), cspace_wrap(MatchString(r'('))),
                Left(Expression(), cspace_wrap(MatchString(r','))),
                Left(Expression(), cspace_wrap(MatchString(r','))),
                Left(Expression(), cspace_wrap(MatchString(r')'))),
            ),
            _parse_call2('STRLANG'),
            _parse_call2('STRDT'),
            _parse_call2('sameTerm'),
            _parse_call1('isIRI'),
            _parse_call1('isURI'),
            _parse_call1('isBLANK'),
            _parse_call1('isLITERAL'),
            _parse_call1('isNUMERIC'),
            RegexExpression(),
            ExistsFunc(),
            NotExistsFunc(),
        )


class RegexExpression(TaggingParser):
    # 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
    def parser(self) -> Parser:
        return Right(
            Pair(MatchICase(r'REGEX'), cspace_wrap(MatchString(r'('))),
            Left(
                Pair(
                    Pair(
                        Expression(),
                        Right(cspace_wrap(MatchString(r',')), Expression()),
                    ),
                    Optional(Right(cspace_wrap(MatchString(r',')), Expression())),
                ),
                MatchString(r')'),
            ),
        )


class SubstringExpression(TaggingParser):
    # 'SUBSTR' '(' Expression ',' Expression ( ',' Expression )? ')'
    def parser(self) -> Parser:
        return Right(
            Pair(MatchICase(r'SUBSTR'), cspace_wrap(MatchString(r'('))),
            Left(
                Pair(
                    Pair(
                        Expression(),
                        Right(cspace_wrap(MatchString(r',')), Expression()),
                    ),
                    Optional(Right(cspace_wrap(MatchString(r',')), Expression())),
                ),
                MatchString(r')'),
            ),
        )


class StrReplaceExpression(TaggingParser):
    # 'REPLACE' '(' Expression ',' Expression ',' Expression ( ',' Expression )? ')'
    def parser(self) -> Parser:
        return Right(
            Pair(MatchICase(r'REPLACE'), MatchString(r'(')),
            Left(
                Pair(
                    Pair(
                        Pair(
                            Expression(),
                            Right(cspace_wrap(MatchString(r',')), Expression()),
                        ),
                        Right(cspace_wrap(MatchString(r',')), Expression()),
                    ),
                    Optional(Right(cspace_wrap(MatchString(r',')), Expression())),
                ),
                MatchString(r')'),
            ),
        )


class ExistsFunc(TaggingParser):
    # 'EXISTS' GroupGraphPattern
    def parser(self) -> Parser:
        return Right(cspace_wrap(MatchICase(r'EXISTS')), GroupGraphPattern())


class NotExistsFunc(TaggingParser):
    # 'NOT' 'EXISTS' GroupGraphPattern
    def parser(self) -> Parser:
        return Right(
            cspace_pattern_of(MatchICase(r'NOT'), MatchICase(r'EXISTS')),
            cspace_wrap(GroupGraphPattern()),
        )


class Aggregate(TaggingParser):
    # 'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')' | 'SUM' '(' 'DISTINCT'? Expression ')' | 'MIN' '(' 'DISTINCT'? Expression ')' | 'MAX' '(' 'DISTINCT'? Expression ')' | 'AVG' '(' 'DISTINCT'? Expression ')' | 'SAMPLE' '(' 'DISTINCT'? Expression ')' | 'GROUP_CONCAT' '(' 'DISTINCT'? Expression ( ';' 'SEPARATOR' '=' String )? ')'
    def parser(self) -> Parser:
        opt_distinct = Optional(MatchICase(r'DISTINCT'))
        return either_of(
            _parse_call1_with(
                'COUNT',
                cspace_pattern_of(
                    opt_distinct,
                    Either(MatchString(r'*'), Expression()),
                ),
            ),
            _parse_call1_with(
                'SUM',
                cspace_pattern_of(opt_distinct, Expression()),
            ),
            _parse_call1_with(
                'MIN',
                cspace_pattern_of(opt_distinct, Expression()),
            ),
            _parse_call1_with(
                'MAX',
                cspace_pattern_of(opt_distinct, Expression()),
            ),
            _parse_call1_with(
                'AVG',
                cspace_pattern_of(opt_distinct, Expression()),
            ),
            _parse_call1_with(
                'SAMPLE',
                cspace_pattern_of(opt_distinct, Expression()),
            ),
            _parse_call1_with(
                'GROUP_CONCAT',
                cspace_pattern_of(
                    opt_distinct,
                    Expression(),
                    Optional(
                        cspace_pattern_of(
                            MatchString(r';'),
                            MatchICase(r'SEPARATOR'),
                            MatchString(r'='),
                            String(),
                        )
                    ),
                ),
            ),
        )


class iriOrFunction(TaggingParser):
    # iri ArgList?
    def parser(self) -> Parser:
        return cspace_pattern_of(iri(), Optional(ArgList()))


class RDFLiteral(TaggingParser):
    """
    >>> LANGTAG.parse('@sv-SE')
    ('', Tagged(name='LANGTAG', value=(('@', ['s', 'v']), [('-', ['S', 'E'])])))
    """

    # String ( LANGTAG | ( '^^' iri ) )?
    def parser(self) -> Parser:
        return cspace_pattern_of(
            String(),
            Optional(Either(LANGTAG, Right(cspace_wrap(MatchString(r'^^')), iri()))),
        )


def NumericLiteral() -> Parser:
    # NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
    return either_of(
        NumericLiteralPositive(), NumericLiteralNegative(), NumericLiteralUnsigned()
    )


def NumericLiteralUnsigned() -> Parser:
    # INTEGER | DECIMAL | DOUBLE
    return either_of(DOUBLE, DECIMAL, INTEGER)


def NumericLiteralPositive() -> Parser:
    # INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE
    return either_of(DOUBLE_POSITIVE, DECIMAL_POSITIVE, INTEGER_POSITIVE)


def NumericLiteralNegative() -> Parser:
    # INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE
    return either_of(DOUBLE_NEGATIVE, DECIMAL_NEGATIVE, INTEGER_NEGATIVE)


class BooleanLiteral(TaggingParser):
    # 'true' | 'false'
    def parser(self) -> Parser:
        return Either(MatchString(r'true'), MatchString(r'false'))


def String() -> Parser:
    """
    >>> String().parse('""')
    ('', '')

    >>> String().parse('"abc"')
    ('', 'abc')
    """
    # STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
    return either_of(
        STRING_LITERAL_LONG1,
        STRING_LITERAL_LONG2,
        STRING_LITERAL1,
        STRING_LITERAL2,
    )


def iri() -> Parser:
    # IRIREF | PrefixedName
    return Either(IRIREF, PrefixedName())


class PrefixedName(TaggingParser):
    """
    >>> parser = PrefixedName()

    >>> parser.parse("a:b <some>")
    (' <some>', Tagged(name='PrefixedName', value=('a:', 'b')))

    >>> parser.parse("a: <other>")
    (' <other>', Tagged(name='PrefixedName', value='a:'))

    >>> parser.parse(":def|")
    ('|', Tagged(name='PrefixedName', value=(':', 'def')))

    >>> parser.parse("")
    Error(on='')
    """

    # PNAME_LN | PNAME_NS
    def parser(self) -> Parser:
        return Either(PNAME_LN, PNAME_NS)


def BlankNode() -> Parser:
    # BLANK_NODE_LABEL | ANON
    return Either(BLANK_NODE_LABEL, ANON)


##
#  Productions for terminals:


_MATCH_IRI = re.compile(r'<([^<>"{}|^`\\\u0000-\u0020]*)>').match


class _IRIRef(Parser[Tagged]):
    """
    >>> parser = _IRIRef()

    >>> parser.parse('<http://example.org/>')
    ('', Tagged(name='IRIREF', value='http://example.org/'))

    >>> parser.parse("<http://example.org/path?query=x&y=z#hash>")
    ('', Tagged(name='IRIREF', value='http://example.org/path?query=x&y=z#hash'))

    >>> parser.parse("<<>")
    Error(on='<<>')

    >>> parser.parse("< >")
    Error(on='< >')

    >>> parser.parse("")
    Error(on='')
    """

    # '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
    def parse(self, input: str) -> ParseResult[Tagged]:
        m = _MATCH_IRI(input)
        if m:
            s = m[1]
            return input[m.end() :], Tagged('IRIREF', s)

        return Error(input)


IRIREF = _IRIRef()


LANGTAG = TaggedParser(
    'LANGTAG',
    pattern_of(
        MatchString(r'@'),
        OneOrMore(RegExp(r'[a-zA-Z]')),
        ZeroOrMore(
            pattern_of(MatchString(r'-'), OneOrMore(RegExp(r'[a-zA-Z0-9]')))
        ),
    )
) # '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*


def _digit_string():
    return CollectString(OneOrMore(RegExp(r'[0-9]')))


def _opt_digit_string():
    return CollectString(ZeroOrMore(RegExp(r'[0-9]')))


INTEGER = TaggedParser('INTEGER', _digit_string()) # [0-9]+

DECIMAL = TaggedParser('DECIMAL',
    pattern_of(_opt_digit_string(), MatchString(r'.'), _digit_string())
) # [0-9]* '.' [0-9]+

EXPONENT = TaggedParser('EXPONENT', 
    pattern_of(RegExp(r'[eE]'), Optional(RegExp(r'[+-]')), _digit_string())
) # [eE] [+-]? [0-9]+

DOUBLE = TaggedParser('DOUBLE',
    either_of(
        pattern_of(
            _digit_string(), MatchString(r'.'), _opt_digit_string(), EXPONENT
        ),
        pattern_of(MatchString(r'.'), _digit_string(), EXPONENT),
        pattern_of(_digit_string(), EXPONENT),
    )
) # [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT

INTEGER_POSITIVE = TaggedParser('INTEGER_POSITIVE',
    Right(MatchString(r'+'), INTEGER)
) # '+' INTEGER

DECIMAL_POSITIVE = TaggedParser('DECIMAL_POSITIVE',
    Right(MatchString(r'+'), DECIMAL)
) # '+' DECIMAL

DOUBLE_POSITIVE = TaggedParser('DOUBLE_POSITIVE',
    Right(MatchString(r'+'), DOUBLE)
) # '+' DOUBLE

INTEGER_NEGATIVE = TaggedParser('INTEGER_NEGATIVE',
    Right(MatchString(r'-'), INTEGER)
) # '-' INTEGER

DECIMAL_NEGATIVE = TaggedParser('DECIMAL_NEGATIVE',
    Right(MatchString(r'-'), DECIMAL)
) # '-' DECIMAL

DOUBLE_NEGATIVE = TaggedParser('DOUBLE_NEGATIVE',
    Right(MatchString(r'-'), DOUBLE)
) # '-' DOUBLE


ECHAR: Parser = Right(MatchString('\\'), RegExp(r'[tbnrf\\\"\']')) # '\' [tbnrf\"']

STRING_LITERAL1: Parser = Right(
    MatchString(r"'"),
    Left(
        ReduceToString(
            ZeroOrMore(either_of(RegExp(r'[^\u0027\u005C\u000A\u000D]'), ECHAR))
        ),
        MatchString(r"'"),
    ),
) # "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
# >>> STRING_LITERAL1.parse("'abc'")
# ('', 'abc')

STRING_LITERAL2: Parser = Right(
    MatchString(r'"'),
    Left(
        ReduceToString(
            ZeroOrMore(either_of(RegExp(r'[^\u0022\u005C\u000A\u000D]'), ECHAR))
        ),
        MatchString(r'"'),
    ),
) # '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
# >>> STRING_LITERAL2().parse('"abc"')
# ('', 'abc')

STRING_LITERAL_LONG1: Parser = Right(
    MatchString(r"'''"),
    Left(
        ReduceToString(
            ZeroOrMore(
                Pair(
                    Optional(Either(MatchString(r"'"), MatchString(r"''"))),
                    Either(RegExp(r"[^'\\]"), ECHAR),
                ),
            ),
        ),
        MatchString(r"'''"),
    ),
) # "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"

STRING_LITERAL_LONG2: Parser = Right(
    MatchString(r'"""'),
    Left(
        ReduceToString(
            ZeroOrMore(
                Pair(
                    Optional(Either(MatchString(r'"'), MatchString(r'""'))),
                    Either(RegExp(r'[^"\\]'), ECHAR),
                ),
            ),
        ),
        MatchString(r'"""'),
    ),
) # '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'


NIL = TaggedParser(
    'NIL', pattern_of(MatchString(r'('), CSPACE0, MatchString(r')'))
) # '(' WS* ')'

# WS = #x20 | #x9 | #xD | #xA

ANON = TaggedParser(
    'ANON',
    # '[' WS* ']'
    pattern_of(MatchString(r'['), CSPACE0, MatchString(r']'))
)


PN_CHARS_BASE = Either(RegExp(r'[A-Z]'), RegExp(r'[a-z]'))
    # either_of(RegExp(r'[A-Z]'), RegExp(r'[a-z]'), RegExp(r'[#x00C0-#x00D6]'), RegExp(r'[#x00D8-#x00F6]'), RegExp(r'[#x00F8-#x02FF]'), RegExp(r'[#x0370-#x037D]'), RegExp(r'[#x037F-#x1FFF]'), RegExp(r'[#x200C-#x200D]'), RegExp(r'[#x2070-#x218F]'), RegExp(r'[#x2C00-#x2FEF]'), RegExp(r'[#x3001-#xD7FF]'), RegExp(r'[#xF900-#xFDCF]'), RegExp(r'[#xFDF0-#xFFFD]'), RegExp(r'[#x10000-#xEFFFF]'))
    # [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]


PN_CHARS_U = Either(PN_CHARS_BASE, MatchString(r'_')) # PN_CHARS_BASE | '_'

PN_CHARS = either_of(PN_CHARS_U, MatchString(r'-'), RegExp(r'[0-9]'))
# either_of(PN_CHARS_U(), MatchString(r'-'), RegExp(r'[0-9]'), Tagged(name='hexchar', value='x00B7'), RegExp(r'[#x0300-#x036F]'), RegExp(r'[#x203F-#x2040]'))
# PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]


PN_PREFIX = ReduceToString(Pair(PN_CHARS_BASE, Optional(ZeroOrMore(PN_CHARS))))
# pattern_of(PN_CHARS_BASE(), Optional(pattern_of(ZeroOrMore(either_of(PN_CHARS(), MatchString(r'.'))), PN_CHARS())))
# PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?

PNAME_NS = ReduceToString(Pair(Optional(PN_PREFIX), MatchString(r':'))) # PN_PREFIX? ':'


class _PN_LOCAL_ESC(TaggingParser):
    # '\' ( '_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%' )
    def parser(self) -> Parser:
        esc_chars = (
            '_',  '~',  '.',  '-',  '!',  '$',  '&',  "'",  '(',  ')',  '*',  '+',  ',',  ';',  '=',  '/',  '?',  '#',  '@',  '%'
        )
        esc = '\\'
        return CollectString(
            Right(MatchString(esc), OneOrMore(AnyChar(lambda c: c in esc_chars)))
        )


PN_LOCAL_ESC = _PN_LOCAL_ESC()

HEX = either_of(RegExp(r'[0-9]'), RegExp(r'[A-F]'), RegExp(r'[a-f]')) # [0-9] | [A-F] | [a-f]

PERCENT = Pair(MatchString(r'%'), Pair(HEX, HEX)) # '%' HEX HEX

PLX = Either(PERCENT, PN_LOCAL_ESC) # PERCENT | PN_LOCAL_ESC

PN_LOCAL = ReduceToString(
    Pair(
        either_of(PN_CHARS_U, MatchString(r':'), RegExp(r'[0-9]'), PLX),
        Optional(
            pattern_of(
                ZeroOrMore(
                    either_of(
                        PN_CHARS, MatchString(r'.'), MatchString(r':'), PLX
                    )
                )
            )
        ),
    )
)


PNAME_LN = Pair(PNAME_NS, PN_LOCAL)

BLANK_NODE_LABEL = TaggedParser(
    'BLANK_NODE_LABEL',
    # '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
    Right(
        MatchString(r"_:"),
        ReduceToString(
            Pair(
                Either(PN_CHARS_U, RegExp(r"[0-9]")),
                Optional(
                    # TODO: this doesn't work
                    # pattern_of(
                    #    ZeroOrMore(Either(PN_CHARS(), MatchString(r"."))), PN_CHARS()
                    # ),
                    ZeroOrMore(PN_CHARS),
                ),
            )
        ),
    )
)

# ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
# return pattern_of(either_of(PN_CHARS_U(), RegExp(r'[0-9]')), ZeroOrMore(either_of(PN_CHARS_U(), RegExp(r'[0-9]'), Tagged(name='hexchar', value='x00B7'), RegExp(r'[#x0300-#x036F]'), RegExp(r'[#x203F-#x2040]'))))
VARNAME = ReduceToString(
    Pair(
        Either(PN_CHARS_U, RegExp(r'[0-9]')),
        ZeroOrMore(either_of(PN_CHARS_U, RegExp(r'[0-9]'))),
    )
)

VAR1 = TaggedParser('VAR1', Right(MatchString(r'?'), VARNAME)) # '?' VARNAME

VAR2 = TaggedParser('VAR2', Right(MatchString(r'$'), VARNAME)) # '$' VARNAME


def parse(indata: str):
    parser: Parser = Either(QueryUnit(), UpdateUnit())
    result = parser.parse(indata)
    if isinstance(result, Error):
        raise Exception(result.on)
    next_input, value = result
    if next_input:
        raise Exception(f'Unable to parse: {next_input[:10]!r}')
    return value


if __name__ == '__main__':
    import doctest

    doctest.testmod()
