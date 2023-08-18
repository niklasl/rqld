import re
from functools import reduce

from .partr.combinators import *
from .partr.tagging import (ParserMaker, RecursiveParser, Tagged, TaggedParser,
                            parser_maker, tagged_parser)


def RegExp(exp: str):
    rexp = re.compile(exp)
    return AnyChar(lambda c: rexp.match(c) is not None)


CSPACE0 = space0('#')

CSPACE1 = space1('#')


def cspace_wrap(parser: Parser) -> Parser:
    return Right(CSPACE0, Left(parser, CSPACE0))


def cspace_pattern_of(*parsers) -> Parser:
    combos: list = list(parsers[:1]) + [Right(CSPACE0, p) for p in parsers[1:]]
    return reduce(Pair, combos)


# QueryUnit ::= Query
QueryUnit = parser_maker(lambda: Query())

# Query ::= Prologue ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery ) ValuesClause
Query: ParserMaker = tagged_parser(
    'Query',
    lambda: cspace_pattern_of(
        Prologue(),
        either_of(SelectQuery(), ConstructQuery(), DescribeQuery(), AskQuery()),
        ValuesClause(),
    ),
)

# UpdateUnit ::= Update
UpdateUnit = parser_maker(lambda: Update)

# Prologue ::= ( BaseDecl | PrefixDecl )*
Prologue: ParserMaker = tagged_parser(
    'Prologue', lambda: ZeroOrMore(cspace_wrap(Either(BaseDecl(), PrefixDecl())))
)

# BaseDecl ::= 'BASE' IRIREF
BaseDecl: ParserMaker = tagged_parser(
    'BaseDecl', lambda: Right(Pair(MatchICase(r'BASE'), CSPACE1), IRIREF)
)

# PrefixDecl ::= 'PREFIX' PNAME_NS IRIREF
PrefixDecl: ParserMaker = tagged_parser(
    'PrefixDecl',
    lambda: Right(
        Pair(MatchICase(r'PREFIX'), CSPACE1),
        Pair(Left(PNAME_NS, CSPACE1), IRIREF),
    ),
)

# SelectQuery ::= SelectClause DatasetClause* WhereClause SolutionModifier
SelectQuery: ParserMaker = tagged_parser(
    'SelectQuery',
    lambda: cspace_pattern_of(
        SelectClause(),
        ZeroOrMore(DatasetClause()),
        WhereClause(),
        SolutionModifier(),
    ),
)

# SubSelect ::= SelectClause WhereClause SolutionModifier ValuesClause
SubSelect: ParserMaker = tagged_parser(
    'SubSelect',
    lambda: cspace_pattern_of(
        SelectClause(), WhereClause(), SolutionModifier(), ValuesClause()
    ),
)

# SelectClause ::= 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )
SelectClause: ParserMaker = tagged_parser(
    'SelectClause',
    lambda: cspace_pattern_of(
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
    ),
)

# ConstructQuery ::= 'CONSTRUCT' ( ConstructTemplate DatasetClause* WhereClause SolutionModifier | DatasetClause* 'WHERE' '{' TriplesTemplate? '}' SolutionModifier )
ConstructQuery: ParserMaker = tagged_parser(
    'ConstructQuery',
    lambda: Right(
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
                        Optional(TriplesTemplate),
                        cspace_wrap(MatchString(r'}')),
                    ),
                ),
                SolutionModifier(),
            ),
        ),
    ),
)

# DescribeQuery ::= 'DESCRIBE' ( VarOrIri+ | '*' ) DatasetClause* WhereClause? SolutionModifier
DescribeQuery: ParserMaker = tagged_parser(
    'DescribeQuery',
    lambda: cspace_pattern_of(
        MatchICase(r'DESCRIBE'),
        either_of(OneOrMore(VarOrIri()), MatchString(r'*')),
        ZeroOrMore(DatasetClause()),
        Optional(WhereClause()),
        SolutionModifier(),
    ),
)

# AskQuery ::= 'ASK' DatasetClause* WhereClause SolutionModifier
AskQuery: ParserMaker = tagged_parser(
    'AskQuery',
    lambda: cspace_pattern_of(
        MatchICase(r'ASK'),
        ZeroOrMore(DatasetClause()),
        WhereClause(),
        SolutionModifier(),
    ),
)

# DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
DatasetClause: ParserMaker = tagged_parser(
    'DatasetClause',
    lambda: cspace_pattern_of(
        MatchICase(r'FROM'), either_of(DefaultGraphClause(), NamedGraphClause())
    ),
)

# DefaultGraphClause ::= SourceSelector
DefaultGraphClause: ParserMaker = tagged_parser(
    'DefaultGraphClause', lambda: SourceSelector()
)

# NamedGraphClause ::= 'NAMED' SourceSelector
NamedGraphClause: ParserMaker = tagged_parser(
    'NamedGraphClause',
    lambda: cspace_pattern_of(MatchICase(r'NAMED'), SourceSelector()),
)

# SourceSelector ::= iri
SourceSelector: ParserMaker = tagged_parser('SourceSelector', lambda: iri())

# WhereClause ::= 'WHERE'? GroupGraphPattern
WhereClause: ParserMaker = parser_maker(
    lambda: Right((Optional(Pair(MatchICase(r'WHERE'), CSPACE0))), GroupGraphPattern)
)

# SolutionModifier ::= GroupClause? HavingClause? OrderClause? LimitOffsetClauses?
SolutionModifier: ParserMaker = tagged_parser(
    'SolutionModifier',
    lambda: cspace_pattern_of(
        Optional(GroupClause()),
        Optional(HavingClause()),
        Optional(OrderClause()),
        Optional(LimitOffsetClauses()),
    ),
)

# GroupClause ::= 'GROUP' 'BY' GroupCondition+
GroupClause: ParserMaker = parser_maker(
    lambda: Right(
        cspace_wrap(cspace_pattern_of(MatchICase(r'GROUP'), MatchICase(r'BY'))),
        OneOrMore(cspace_wrap(GroupCondition())),
    )
)

# GroupCondition ::= BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var
GroupCondition: ParserMaker = tagged_parser(
    'GroupCondition',
    lambda: either_of(
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
    ),
)

# HavingClause ::= 'HAVING' HavingCondition+
HavingClause: ParserMaker = parser_maker(
    lambda: Right(MatchICase(r'HAVING'), OneOrMore(cspace_wrap(HavingCondition())))
)

# HavingCondition ::= Constraint
HavingCondition: ParserMaker = parser_maker(lambda: Constraint())

# OrderClause ::= 'ORDER' 'BY' OrderCondition+
OrderClause: ParserMaker = parser_maker(
    lambda: Right(
        cspace_wrap(cspace_pattern_of(MatchICase(r'ORDER'), MatchICase(r'BY'))),
        OneOrMore(cspace_wrap(OrderCondition())),
    )
)

# OrderCondition ::= ( ( 'ASC' | 'DESC' ) BrackettedExpression ) | ( Constraint | Var )
OrderCondition: ParserMaker = tagged_parser(
    'OrderCondition',
    lambda: Either(
        cspace_pattern_of(
            Either(MatchICase(r'ASC'), MatchICase(r'DESC')), BrackettedExpression()
        ),
        Either(Constraint(), Var()),
    ),
)

# LimitOffsetClauses ::= LimitClause OffsetClause? | OffsetClause LimitClause?
LimitOffsetClauses: ParserMaker = parser_maker(
    lambda: Either(
        cspace_pattern_of(LimitClause(), Optional(OffsetClause())),
        cspace_pattern_of(OffsetClause(), Optional(LimitClause())),
    )
)

# LimitClause ::= 'LIMIT' INTEGER
LimitClause: ParserMaker = tagged_parser(
    'LimitClause', lambda: Right(cspace_wrap(MatchICase(r'LIMIT')), INTEGER)
)

# OffsetClause ::= 'OFFSET' INTEGER
OffsetClause: ParserMaker = tagged_parser(
    'OffsetClause', lambda: Right(cspace_wrap(MatchICase(r'OFFSET')), INTEGER)
)

# ValuesClause ::= ( 'VALUES' DataBlock )?
ValuesClause: ParserMaker = parser_maker(
    lambda: Optional(Right(cspace_wrap(MatchICase(r'VALUES')), DataBlock()))
)

# Update ::= Prologue ( Update1 ( ';' Update )? )?
Update: Parser = RecursiveParser(
    'Update',
    lambda: pattern_of(
        Prologue(),
        Optional(
            cspace_pattern_of(
                Update1(), Optional(Right(cspace_wrap(MatchString(r';')), Update))
            )
        ),
    ),
)

# Update1 ::= Load | Clear | Drop | Add | Move | Copy | Create | InsertData | DeleteData | DeleteWhere | Modify
Update1: ParserMaker = parser_maker(
    lambda: either_of(
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
)

# Load ::= 'LOAD' 'SILENT'? iri ( 'INTO' GraphRef )?
Load: ParserMaker = tagged_parser(
    'Load',
    lambda: cspace_pattern_of(
        MatchICase(r'LOAD'),
        Optional(MatchICase(r'SILENT')),
        iri(),
        Optional(cspace_pattern_of(MatchICase(r'INTO'), GraphRef())),
    ),
)

# Clear ::= 'CLEAR' 'SILENT'? GraphRefAll
Clear: ParserMaker = tagged_parser(
    'Clear',
    lambda: cspace_pattern_of(
        MatchICase(r'CLEAR'), Optional(MatchICase(r'SILENT')), GraphRefAll()
    ),
)

# Drop ::= 'DROP' 'SILENT'? GraphRefAll
Drop: ParserMaker = tagged_parser(
    'Drop',
    lambda: cspace_pattern_of(
        MatchICase(r'DROP'), Optional(MatchICase(r'SILENT')), GraphRefAll()
    ),
)

# Create ::= 'CREATE' 'SILENT'? GraphRef
Create: ParserMaker = tagged_parser(
    'Create',
    lambda: cspace_pattern_of(
        MatchICase(r'CREATE'), Optional(MatchICase(r'SILENT')), GraphRef()
    ),
)

# Add ::= 'ADD' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
Add: ParserMaker = tagged_parser(
    'Add',
    lambda: cspace_pattern_of(
        MatchICase(r'ADD'),
        Optional(MatchICase(r'SILENT')),
        GraphOrDefault(),
        MatchICase(r'TO'),
        GraphOrDefault(),
    ),
)

# Move ::= 'MOVE' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
Move: ParserMaker = tagged_parser(
    'Move',
    lambda: cspace_pattern_of(
        MatchICase(r'MOVE'),
        Optional(MatchICase(r'SILENT')),
        GraphOrDefault(),
        MatchICase(r'TO'),
        GraphOrDefault(),
    ),
)

# Copy ::= 'COPY' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
Copy: ParserMaker = tagged_parser(
    'Copy',
    lambda: cspace_pattern_of(
        MatchICase(r'COPY'),
        Optional(MatchICase(r'SILENT')),
        GraphOrDefault(),
        MatchICase(r'TO'),
        GraphOrDefault(),
    ),
)

# InsertData ::= 'INSERT DATA' QuadData
InsertData: ParserMaker = tagged_parser(
    'InsertData', lambda: cspace_pattern_of(MatchString(r'INSERT DATA'), QuadData())
)

# DeleteData ::= 'DELETE DATA' QuadData
DeleteData: ParserMaker = tagged_parser(
    'DeleteData', lambda: cspace_pattern_of(MatchString(r'DELETE DATA'), QuadData())
)

# DeleteWhere ::= 'DELETE WHERE' QuadPattern
DeleteWhere: ParserMaker = tagged_parser(
    'DeleteWhere',
    lambda: cspace_pattern_of(MatchString(r'DELETE WHERE'), QuadPattern()),
)

# Modify ::= ( 'WITH' iri )? ( DeleteClause InsertClause? | InsertClause ) UsingClause* 'WHERE' GroupGraphPattern
Modify: ParserMaker = tagged_parser(
    'Modify',
    lambda: cspace_pattern_of(
        Optional(Right(cspace_wrap(MatchICase(r"WITH")), iri())),
        Either(
            pattern_of(DeleteClause(), Optional(InsertClause())),
            InsertClause(),
        ),
        ZeroOrMore(UsingClause()),
        Right(cspace_wrap(MatchICase(r"WHERE")), GroupGraphPattern),
    ),
)

# DeleteClause ::= 'DELETE' QuadPattern
DeleteClause: ParserMaker = tagged_parser(
    'DeleteClause', lambda: Right(cspace_wrap(MatchICase(r'DELETE')), QuadPattern())
)

# InsertClause ::= 'INSERT' QuadPattern
InsertClause: ParserMaker = tagged_parser(
    'InsertClause', lambda: Right(cspace_wrap(MatchICase(r'INSERT')), QuadPattern())
)

# UsingClause ::= 'USING' ( iri | 'NAMED' iri )
UsingClause: ParserMaker = tagged_parser(
    'UsingClause',
    lambda: Right(
        cspace_wrap(MatchICase(r'USING')),
        Either(iri(), Right(cspace_wrap(MatchICase(r'NAMED')), iri())),
    ),
)

# GraphOrDefault ::= 'DEFAULT' | 'GRAPH'? iri
GraphOrDefault: ParserMaker = tagged_parser(
    'GraphOrDefault',
    lambda: Either(
        MatchICase(r'DEFAULT'),
        cspace_pattern_of(Optional(MatchICase(r'GRAPH')), iri()),
    ),
)

# GraphRef ::= 'GRAPH' iri
GraphRef: ParserMaker = tagged_parser(
    'GraphRef', lambda: cspace_pattern_of(MatchICase(r'GRAPH'), iri())
)

# GraphRefAll ::= GraphRef | 'DEFAULT' | 'NAMED' | 'ALL'
GraphRefAll: ParserMaker = tagged_parser(
    'GraphRefAll',
    lambda: either_of(
        GraphRef(), MatchICase(r'DEFAULT'), MatchICase(r'NAMED'), MatchICase(r'ALL')
    ),
)

# QuadPattern ::= '{' Quads '}'
QuadPattern: ParserMaker = parser_maker(
    lambda: Right(
        cspace_wrap(MatchString(r'{')), Left(Quads(), cspace_wrap(MatchString(r'}')))
    )
)

# QuadData ::= '{' Quads '}'
QuadData: ParserMaker = parser_maker(lambda: QuadPattern())

# Quads ::= TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
Quads: ParserMaker = tagged_parser(
    'Quads',
    lambda: cspace_pattern_of(
        Optional(TriplesTemplate),
        ZeroOrMore(
            cspace_pattern_of(
                Left(QuadsNotTriples(), Optional(cspace_wrap(MatchString(r".")))),
                Optional(TriplesTemplate),
            )
        ),
    ),
)

# QuadsNotTriples ::= 'GRAPH' VarOrIri '{' TriplesTemplate? '}'
QuadsNotTriples: ParserMaker = tagged_parser(
    'QuadsNotTriples',
    lambda: cspace_pattern_of(
        Right(cspace_wrap(MatchICase(r"GRAPH")), VarOrIri()),
        Right(
            cspace_wrap(MatchString(r"{")),
            Left(Optional(TriplesTemplate), cspace_wrap(MatchString(r"}"))),
        ),
    ),
)

# TriplesTemplate ::= TriplesSameSubject ( '.' TriplesTemplate? )?
TriplesTemplate: Parser = RecursiveParser(
    'TriplesTemplate',
    lambda: Pair(
        TriplesSameSubject(),
        Optional(Right(cspace_wrap(MatchString(r'.')), Optional(TriplesTemplate))),
    ),
)

# GroupGraphPattern ::= '{' ( SubSelect | GroupGraphPatternSub ) '}'
GroupGraphPattern: Parser = RecursiveParser(
    'GroupGraphPattern',
    lambda: Right(
        Pair(MatchString(r'{'), CSPACE0),
        Left(
            Either(SubSelect(), GroupGraphPatternSub()),
            Pair(CSPACE0, MatchString(r'}')),
        ),
    ),
)

# GroupGraphPatternSub ::= TriplesBlock? ( GraphPatternNotTriples '.'? TriplesBlock? )*
GroupGraphPatternSub: ParserMaker = tagged_parser(
    'GroupGraphPatternSub',
    lambda: cspace_pattern_of(
        Optional(TriplesBlock),
        ZeroOrMore(
            cspace_pattern_of(
                Left(
                    GraphPatternNotTriples(),
                    cspace_wrap(Optional(MatchString(r'.'))),
                ),
                Optional(TriplesBlock),
            )
        ),
    ),
)

# TriplesBlock ::= TriplesSameSubjectPath ( '.' TriplesBlock? )?
TriplesBlock: Parser = RecursiveParser(
    'TriplesBlock',
    lambda: cspace_pattern_of(
        TriplesSameSubjectPath(),
        Optional(Right(cspace_wrap(MatchString(r'.')), Optional(TriplesBlock))),
    ),
)

# GraphPatternNotTriples ::= GroupOrUnionGraphPattern | OptionalGraphPattern | MinusGraphPattern | GraphGraphPattern | ServiceGraphPattern | Filter | Bind | InlineData
GraphPatternNotTriples: ParserMaker = parser_maker(
    lambda: either_of(
        GroupOrUnionGraphPattern(),
        OptionalGraphPattern(),
        MinusGraphPattern(),
        GraphGraphPattern(),
        ServiceGraphPattern(),
        Filter(),
        Bind(),
        InlineData(),
    )
)

# OptionalGraphPattern ::= 'OPTIONAL' GroupGraphPattern
OptionalGraphPattern: ParserMaker = tagged_parser(
    'OptionalGraphPattern',
    lambda: Right(cspace_wrap(MatchICase(r'OPTIONAL')), GroupGraphPattern),
)

# GraphGraphPattern ::= 'GRAPH' VarOrIri GroupGraphPattern
GraphGraphPattern: ParserMaker = tagged_parser(
    'GraphGraphPattern',
    lambda: Right(
        cspace_wrap(MatchICase(r'GRAPH')),
        cspace_pattern_of(VarOrIri(), GroupGraphPattern),
    ),
)

# ServiceGraphPattern ::= 'SERVICE' 'SILENT'? VarOrIri GroupGraphPattern
ServiceGraphPattern: ParserMaker = tagged_parser(
    'ServiceGraphPattern',
    lambda: Right(
        cspace_wrap(MatchICase(r'SERVICE')),
        cspace_pattern_of(
            Optional(MatchICase(r'SILENT')),
            VarOrIri(),
            GroupGraphPattern,
        ),
    ),
)

# Bind ::= 'BIND' '(' Expression 'AS' Var ')'
Bind: ParserMaker = tagged_parser(
    'Bind',
    lambda: cspace_pattern_of(
        Right(
            cspace_pattern_of(MatchICase(r"BIND"), MatchString(r"(")),
            Expression(),
        ),
        Left(
            Right(cspace_wrap(MatchICase(r"AS")), Var()),
            cspace_wrap(MatchString(r")")),
        ),
    ),
)

# InlineData ::= 'VALUES' DataBlock
InlineData: ParserMaker = tagged_parser(
    'InlineData', lambda: Right(cspace_wrap(MatchICase(r'VALUES')), DataBlock())
)

# DataBlock ::= InlineDataOneVar | InlineDataFull
DataBlock: ParserMaker = parser_maker(
    lambda: Either(InlineDataOneVar(), InlineDataFull())
)

# InlineDataOneVar ::= Var '{' DataBlockValue* '}'
InlineDataOneVar: ParserMaker = tagged_parser(
    'InlineDataOneVar',
    lambda: cspace_pattern_of(
        Var(),
        Right(
            cspace_wrap(MatchString(r'{')),
            Left(
                ZeroOrMore(cspace_wrap(DataBlockValue())),
                cspace_wrap(MatchString(r'}')),
            ),
        ),
    ),
)

# InlineDataFull ::= ( NIL | '(' Var* ')' ) '{' ( '(' DataBlockValue* ')' | NIL )* '}'
InlineDataFull: ParserMaker = tagged_parser(
    'InlineDataFull',
    lambda: cspace_pattern_of(
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
    ),
)

# DataBlockValue ::= iri | RDFLiteral | NumericLiteral | BooleanLiteral | 'UNDEF'
DataBlockValue: ParserMaker = parser_maker(
    lambda: either_of(
        iri(), RDFLiteral(), NumericLiteral(), BooleanLiteral(), MatchICase(r'UNDEF')
    )
)

# MinusGraphPattern ::= 'MINUS' GroupGraphPattern
MinusGraphPattern: ParserMaker = tagged_parser(
    'MinusGraphPattern',
    lambda: cspace_pattern_of(MatchICase(r'MINUS'), GroupGraphPattern),
)

# GroupOrUnionGraphPattern ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
GroupOrUnionGraphPattern: ParserMaker = tagged_parser(
    'GroupOrUnionGraphPattern',
    lambda: cspace_pattern_of(
        GroupGraphPattern,
        ZeroOrMore(Right(cspace_wrap(MatchICase(r"UNION")), GroupGraphPattern)),
    ),
)

# Filter ::= 'FILTER' Constraint
Filter: ParserMaker = tagged_parser(
    'Filter', lambda: Right(cspace_wrap(MatchICase(r'FILTER')), Constraint())
)

# Constraint ::= BrackettedExpression | BuiltInCall | FunctionCall
Constraint: ParserMaker = parser_maker(
    lambda: either_of(BrackettedExpression(), BuiltInCall(), FunctionCall())
)

# FunctionCall ::= iri ArgList
FunctionCall: ParserMaker = tagged_parser(
    'FunctionCall', lambda: cspace_pattern_of(iri(), ArgList())
)

# ArgList ::= NIL | '(' 'DISTINCT'? Expression ( ',' Expression )* ')'
ArgList: ParserMaker = tagged_parser(
    'ArgList',
    lambda: Either(
        NIL,
        cspace_pattern_of(
            MatchString(r'('),
            Optional(MatchICase(r'DISTINCT')),
            Expression(),
            ZeroOrMore(cspace_pattern_of(MatchString(r','), Expression())),
            MatchString(r')'),
        ),
    ),
)

# ExpressionList ::= NIL | '(' Expression ( ',' Expression )* ')'
ExpressionList: ParserMaker = tagged_parser(
    'ExpressionList',
    lambda: Either(
        NIL,
        Pair(
            Right(cspace_wrap(MatchString(r"(")), Expression()),
            Left(
                ZeroOrMore(Right(cspace_wrap(MatchString(r",")), Expression())),
                cspace_wrap(MatchString(r")")),
            ),
        ),
    ),
)

# ConstructTemplate ::= '{' ConstructTriples? '}'
ConstructTemplate: ParserMaker = parser_maker(
    lambda: Right(
        Pair(MatchString(r'{'), CSPACE0),
        Left(Optional(ConstructTriples), Pair(CSPACE0, MatchString(r'}'))),
    )
)

# ConstructTriples ::= TriplesSameSubject ( '.' ConstructTriples? )?
ConstructTriples: Parser = RecursiveParser(
    'ConstructTriples',
    lambda: cspace_pattern_of(
        TriplesSameSubject(),
        Optional(Right(MatchString(r'.'), Optional(ConstructTriples))),
    ),
)

# TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty | TriplesNode PropertyList
TriplesSameSubject: ParserMaker = tagged_parser(
    'TriplesSameSubject',
    lambda: either_of(
        cspace_pattern_of(VarOrTerm(), PropertyListNotEmpty()),
        cspace_pattern_of(TriplesNode(), PropertyList()),
    ),
)

# PropertyList ::= PropertyListNotEmpty?
PropertyList: ParserMaker = tagged_parser(
    'PropertyList', lambda: Optional(PropertyListNotEmpty())
)

# PropertyListNotEmpty ::= Verb ObjectList ( ';' ( Verb ObjectList )? )*
PropertyListNotEmpty: ParserMaker = tagged_parser(
    'PropertyListNotEmpty',
    lambda: cspace_pattern_of(
        Verb(),
        ObjectList(),
        ZeroOrMore(
            Right(
                Pair(MatchString(r';'), CSPACE0),
                Optional(cspace_pattern_of(Verb(), ObjectList())),
            )
        ),
    ),
)

# Verb ::= VarOrIri | 'a'
Verb: ParserMaker = tagged_parser(
    'Verb', lambda: either_of(VarOrIri(), MatchString(r'a'))
)

# ObjectList ::= Object ( ',' Object )*
ObjectList: ParserMaker = tagged_parser(
    'ObjectList',
    lambda: cspace_pattern_of(
        Object(), ZeroOrMore(Right(cspace_wrap(MatchString(r',')), Object()))
    ),
)

# Object ::= GraphNode
Object: ParserMaker = parser_maker(lambda: GraphNode())

# TriplesSameSubjectPath ::= VarOrTerm PropertyListPathNotEmpty | TriplesNodePath PropertyListPath
TriplesSameSubjectPath: ParserMaker = tagged_parser(
    'TriplesSameSubjectPath',
    lambda: Either(
        cspace_pattern_of(VarOrTerm(), PropertyListPathNotEmpty()),
        cspace_pattern_of(TriplesNodePath(), PropertyListPath()),
    ),
)

# PropertyListPath ::= PropertyListPathNotEmpty?
PropertyListPath: ParserMaker = tagged_parser(
    'PropertyListPath', lambda: Optional(PropertyListPathNotEmpty())
)

# PropertyListPathNotEmpty ::= ( VerbPath | VerbSimple ) ObjectListPath ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
PropertyListPathNotEmpty: ParserMaker = tagged_parser(
    'PropertyListPathNotEmpty',
    lambda: cspace_pattern_of(
        Either(VerbPath(), VerbSimple()),
        ObjectListPath(),
        ZeroOrMore(
            Right(
                cspace_wrap(MatchString(r';')),
                Optional(
                    cspace_pattern_of(either_of(VerbPath(), VerbSimple()), ObjectList())
                ),
            )
        ),
    ),
)

# VerbPath ::= Path
VerbPath: ParserMaker = parser_maker(lambda: Path())

# VerbSimple ::= Var
VerbSimple: ParserMaker = parser_maker(lambda: Var())

# ObjectListPath ::= ObjectPath ( ',' ObjectPath )*
ObjectListPath: ParserMaker = tagged_parser(
    'ObjectListPath',
    lambda: cspace_pattern_of(
        ObjectPath(),
        ZeroOrMore(Right(cspace_wrap(MatchString(r',')), ObjectPath())),
    ),
)

# ObjectPath ::= GraphNodePath
ObjectPath: ParserMaker = parser_maker(lambda: GraphNodePath())

# Path ::= PathAlternative
Path: ParserMaker = parser_maker(lambda: PathAlternative())

# PathAlternative ::= PathSequence ( '|' PathSequence )*
PathAlternative: ParserMaker = tagged_parser(
    'PathAlternative',
    lambda: cspace_pattern_of(
        PathSequence(),
        ZeroOrMore(Right(cspace_wrap(MatchString(r'|')), PathSequence())),
    ),
)

# PathSequence ::= PathEltOrInverse ( '/' PathEltOrInverse )*
PathSequence: ParserMaker = tagged_parser(
    'PathSequence',
    lambda: cspace_pattern_of(
        PathEltOrInverse(),
        ZeroOrMore(Right(cspace_wrap(MatchString(r'/')), PathEltOrInverse())),
    ),
)

# PathElt ::= PathPrimary PathMod?
PathElt: ParserMaker = tagged_parser(
    'PathElt', lambda: Pair(PathPrimary, Optional(PathMod()))
)

# PathEltOrInverse ::= PathElt | '^' PathElt
PathEltOrInverse: ParserMaker = tagged_parser(
    'PathEltOrInverse',
    lambda: Either(PathElt(), cspace_pattern_of(MatchString(r'^'), PathElt())),
)

# PathMod ::= '?' | '*' | '+'
PathMod: ParserMaker = parser_maker(
    lambda: either_of(MatchString(r'?'), MatchString(r'*'), MatchString(r'+'))
)

# PathPrimary ::= iri | 'a' | '!' PathNegatedPropertySet | '(' Path ')'
PathPrimary: Parser = RecursiveParser(
    'PathPrimary',
    lambda: either_of(
        iri(),
        MatchString(r'a'),
        Right(Pair(MatchString(r'!'), CSPACE0), PathNegatedPropertySet()),
        Left(
            Right(Pair(MatchString(r'('), CSPACE0), Path()),
            Pair(CSPACE0, MatchString(r')')),
        ),
    ),
)

# PathNegatedPropertySet ::= PathOneInPropertySet | '(' ( PathOneInPropertySet ( '|' PathOneInPropertySet )* )? ')'
PathNegatedPropertySet: ParserMaker = tagged_parser(
    'PathNegatedPropertySet',
    lambda: Either(
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
    ),
)

# PathOneInPropertySet ::= iri | 'a' | '^' ( iri | 'a' )
PathOneInPropertySet: ParserMaker = tagged_parser(
    'PathOneInPropertySet',
    lambda: either_of(
        iri(),
        MatchString(r'a'),
        cspace_pattern_of(MatchString(r'^'), either_of(iri(), MatchString(r'a'))),
    ),
)

# Integer ::= INTEGER
Integer: ParserMaker = tagged_parser('Integer', lambda: INTEGER)

# TriplesNode ::= Collection | BlankNodePropertyList
TriplesNode: ParserMaker = parser_maker(
    lambda: Either(Collection, BlankNodePropertyList)
)

# BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
BlankNodePropertyList: Parser = RecursiveParser(
    'BlankNodePropertyList',
    lambda: Left(
        Right(cspace_wrap(MatchString(r'[')), PropertyListNotEmpty()),
        MatchString(r']'),
    ),
)

# TriplesNodePath ::= CollectionPath | BlankNodePropertyListPath
TriplesNodePath: ParserMaker = parser_maker(
    lambda: Either(CollectionPath, BlankNodePropertyListPath)
)

# BlankNodePropertyListPath ::= '[' PropertyListPathNotEmpty ']'
BlankNodePropertyListPath: Parser = RecursiveParser(
    'BlankNodePropertyListPath',
    lambda: Right(
        cspace_wrap(MatchString(r'[')),
        Left(
            PropertyListPathNotEmpty(),
            cspace_wrap(MatchString(r']')),
        ),
    ),
)

# Collection ::= '(' GraphNode+ ')'
Collection: Parser = RecursiveParser(
    'Collection',
    lambda: Right(
        MatchString(r'('),
        Left(OneOrMore(cspace_wrap(GraphNode())), MatchString(r')')),
    ),
)

# CollectionPath ::= '(' GraphNodePath+ ')'
CollectionPath: Parser = RecursiveParser(
    'CollectionPath',
    lambda: Right(
        MatchString(r'('),
        Left(OneOrMore(cspace_wrap(GraphNodePath())), MatchString(r')')),
    ),
)

# GraphNode ::= VarOrTerm | TriplesNode
GraphNode: ParserMaker = parser_maker(lambda: Either(VarOrTerm(), TriplesNode()))

# GraphNodePath ::= VarOrTerm | TriplesNodePath
GraphNodePath: ParserMaker = parser_maker(
    lambda: Either(VarOrTerm(), TriplesNodePath())
)

# VarOrTerm ::= Var | GraphTerm
VarOrTerm: ParserMaker = parser_maker(lambda: Either(Var(), GraphTerm()))

# VarOrIri ::= Var | iri
VarOrIri: ParserMaker = parser_maker(lambda: Either(Var(), iri()))

# Var ::= VAR1 | VAR2
Var: ParserMaker = parser_maker(lambda: Either(VAR1, VAR2))

# GraphTerm ::= iri | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
GraphTerm: ParserMaker = parser_maker(
    lambda: either_of(
        iri(), RDFLiteral(), NumericLiteral(), BooleanLiteral(), BlankNode(), NIL
    )
)

# Expression ::= ConditionalOrExpression
Expression: ParserMaker = parser_maker(lambda: ConditionalOrExpression())

# ConditionalOrExpression ::= ConditionalAndExpression ( '||' ConditionalAndExpression )*
ConditionalOrExpression: ParserMaker = tagged_parser(
    'ConditionalOrExpression',
    lambda: Pair(
        Left(ConditionalAndExpression(), CSPACE0),
        ZeroOrMore(Right(cspace_wrap(MatchString(r'||')), ConditionalAndExpression())),
    ),
)

# ConditionalAndExpression ::= ValueLogical ( '&&' ValueLogical )*
ConditionalAndExpression: ParserMaker = tagged_parser(
    'ConditionalAndExpression',
    lambda: Pair(
        Left(ValueLogical(), CSPACE0),
        ZeroOrMore(Right(cspace_wrap(MatchString(r'&&')), ValueLogical())),
    ),
)

# ValueLogical ::= RelationalExpression
ValueLogical: ParserMaker = parser_maker(lambda: RelationalExpression)

# RelationalExpression ::= NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression | 'IN' ExpressionList | 'NOT' 'IN' ExpressionList )?
RelationalExpression: Parser = RecursiveParser(
    'RelationalExpression',
    lambda: cspace_pattern_of(
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
    ),
)

# NumericExpression ::= AdditiveExpression
NumericExpression: ParserMaker = parser_maker(lambda: AdditiveExpression())

# AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | ( NumericLiteralPositive | NumericLiteralNegative ) ( ( '*' UnaryExpression ) | ( '/' UnaryExpression ) )* )*
AdditiveExpression: ParserMaker = tagged_parser(
    'AdditiveExpression',
    lambda: cspace_pattern_of(
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
    ),
)

# MultiplicativeExpression ::= UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
MultiplicativeExpression: ParserMaker = tagged_parser(
    'MultiplicativeExpression',
    lambda: cspace_pattern_of(
        UnaryExpression(),
        ZeroOrMore(
            Either(
                cspace_pattern_of(MatchString(r'*'), UnaryExpression()),
                cspace_pattern_of(MatchString(r'/'), UnaryExpression()),
            )
        ),
    ),
)

# UnaryExpression ::= '!' PrimaryExpression | '+' PrimaryExpression | '-' PrimaryExpression | PrimaryExpression
UnaryExpression: ParserMaker = tagged_parser(
    'UnaryExpression',
    lambda: either_of(
        cspace_pattern_of(MatchString(r'!'), PrimaryExpression()),
        cspace_pattern_of(MatchString(r'+'), PrimaryExpression()),
        cspace_pattern_of(MatchString(r'-'), PrimaryExpression()),
        PrimaryExpression(),
    ),
)

# PrimaryExpression ::= BrackettedExpression | BuiltInCall | iriOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
PrimaryExpression: ParserMaker = parser_maker(
    lambda: either_of(
        BrackettedExpression(),
        BuiltInCall(),
        iriOrFunction(),
        RDFLiteral(),
        NumericLiteral(),
        BooleanLiteral(),
        Var(),
    )
)

# BrackettedExpression ::= '(' Expression ')'
BrackettedExpression: ParserMaker = parser_maker(
    lambda: Right(
        Pair(MatchString(r'('), CSPACE0),
        Left(Expression(), Pair(CSPACE0, MatchString(r')'))),
    )
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


# BuiltInCall ::= Aggregate | 'STR' '(' Expression ')' | 'LANG' '(' Expression ')' | 'LANGMATCHES' '(' Expression ',' Expression ')' | 'DATATYPE' '(' Expression ')' | 'BOUND' '(' Var ')' | 'IRI' '(' Expression ')' | 'URI' '(' Expression ')' | 'BNODE' ( '(' Expression ')' | NIL ) | 'RAND' NIL | 'ABS' '(' Expression ')' | 'CEIL' '(' Expression ')' | 'FLOOR' '(' Expression ')' | 'ROUND' '(' Expression ')' | 'CONCAT' ExpressionList | SubstringExpression | 'STRLEN' '(' Expression ')' | StrReplaceExpression | 'UCASE' '(' Expression ')' | 'LCASE' '(' Expression ')' | 'ENCODE_FOR_URI' '(' Expression ')' | 'CONTAINS' '(' Expression ',' Expression ')' | 'STRSTARTS' '(' Expression ',' Expression ')' | 'STRENDS' '(' Expression ',' Expression ')' | 'STRBEFORE' '(' Expression ',' Expression ')' | 'STRAFTER' '(' Expression ',' Expression ')' | 'YEAR' '(' Expression ')' | 'MONTH' '(' Expression ')' | 'DAY' '(' Expression ')' | 'HOURS' '(' Expression ')' | 'MINUTES' '(' Expression ')' | 'SECONDS' '(' Expression ')' | 'TIMEZONE' '(' Expression ')' | 'TZ' '(' Expression ')' | 'NOW' NIL | 'UUID' NIL | 'STRUUID' NIL | 'MD5' '(' Expression ')' | 'SHA1' '(' Expression ')' | 'SHA256' '(' Expression ')' | 'SHA384' '(' Expression ')' | 'SHA512' '(' Expression ')' | 'COALESCE' ExpressionList | 'IF' '(' Expression ',' Expression ',' Expression ')' | 'STRLANG' '(' Expression ',' Expression ')' | 'STRDT' '(' Expression ',' Expression ')' | 'sameTerm' '(' Expression ',' Expression ')' | 'isIRI' '(' Expression ')' | 'isURI' '(' Expression ')' | 'isBLANK' '(' Expression ')' | 'isLITERAL' '(' Expression ')' | 'isNUMERIC' '(' Expression ')' | RegexExpression | ExistsFunc | NotExistsFunc
BuiltInCall: ParserMaker = tagged_parser(
    'BuiltInCall',
    lambda: either_of(
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
    ),
)

# RegexExpression ::= 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
RegexExpression: ParserMaker = tagged_parser(
    'RegexExpression',
    lambda: Right(
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
    ),
)

# SubstringExpression ::= 'SUBSTR' '(' Expression ',' Expression ( ',' Expression )? ')'
SubstringExpression: ParserMaker = tagged_parser(
    'SubstringExpression',
    lambda: Right(
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
    ),
)

# StrReplaceExpression ::= 'REPLACE' '(' Expression ',' Expression ',' Expression ( ',' Expression )? ')'
StrReplaceExpression: ParserMaker = tagged_parser(
    'StrReplaceExpression',
    lambda: Right(
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
    ),
)

# ExistsFunc ::= 'EXISTS' GroupGraphPattern
ExistsFunc: ParserMaker = tagged_parser(
    'ExistsFunc', lambda: Right(cspace_wrap(MatchICase(r'EXISTS')), GroupGraphPattern)
)

# NotExistsFunc ::= 'NOT' 'EXISTS' GroupGraphPattern
NotExistsFunc: ParserMaker = tagged_parser(
    'NotExistsFunc',
    lambda: Right(
        cspace_pattern_of(MatchICase(r'NOT'), MatchICase(r'EXISTS')),
        cspace_wrap(GroupGraphPattern),
    ),
)

opt_distinct = Optional(MatchICase(r'DISTINCT'))

# Aggregate ::= 'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')' | 'SUM' '(' 'DISTINCT'? Expression ')' | 'MIN' '(' 'DISTINCT'? Expression ')' | 'MAX' '(' 'DISTINCT'? Expression ')' | 'AVG' '(' 'DISTINCT'? Expression ')' | 'SAMPLE' '(' 'DISTINCT'? Expression ')' | 'GROUP_CONCAT' '(' 'DISTINCT'? Expression ( ';' 'SEPARATOR' '=' String )? ')'
Aggregate: ParserMaker = tagged_parser(
    'Aggregate',
    lambda: either_of(
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
    ),
)

# iriOrFunction ::= iri ArgList?
iriOrFunction: ParserMaker = tagged_parser(
    'iriOrFunction', lambda: cspace_pattern_of(iri(), Optional(ArgList()))
)

# RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
RDFLiteral: ParserMaker = tagged_parser(
    'RDFLiteral',
    lambda: cspace_pattern_of(
        String(),
        Optional(Either(LANGTAG, Right(cspace_wrap(MatchString(r'^^')), iri()))),
    ),
)

# NumericLiteral ::= NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
NumericLiteral: ParserMaker = parser_maker(
    lambda: either_of(
        NumericLiteralPositive(), NumericLiteralNegative(), NumericLiteralUnsigned()
    )
)

# NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
NumericLiteralUnsigned: ParserMaker = parser_maker(
    lambda: either_of(DOUBLE, DECIMAL, INTEGER)
)

# NumericLiteralPositive ::= INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE
NumericLiteralPositive: ParserMaker = parser_maker(
    lambda: either_of(DOUBLE_POSITIVE, DECIMAL_POSITIVE, INTEGER_POSITIVE)
)

# NumericLiteralNegative ::= INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE
NumericLiteralNegative: ParserMaker = parser_maker(
    lambda: either_of(DOUBLE_NEGATIVE, DECIMAL_NEGATIVE, INTEGER_NEGATIVE)
)

# BooleanLiteral ::= 'true' | 'false'
BooleanLiteral: ParserMaker = tagged_parser(
    'BooleanLiteral', lambda: Either(MatchString(r'true'), MatchString(r'false'))
)

# String ::= STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
String: ParserMaker = parser_maker(
    lambda: either_of(
        STRING_LITERAL_LONG1,
        STRING_LITERAL_LONG2,
        STRING_LITERAL1,
        STRING_LITERAL2,
    )
)

# iri ::= IRIREF | PrefixedName
iri: ParserMaker = parser_maker(lambda: Either(IRIREF, PrefixedName()))

# PrefixedName ::= PNAME_LN | PNAME_NS
PrefixedName: ParserMaker = tagged_parser(
    'PrefixedName', lambda: Either(PNAME_LN, PNAME_NS)
)

# BlankNode ::= BLANK_NODE_LABEL | ANON
BlankNode: ParserMaker = parser_maker(lambda: Either(BLANK_NODE_LABEL, ANON))

##
#  Productions for terminals:

_MATCH_IRI = re.compile(r'<([^<>"{}|^`\\\u0000-\u0020]*)>').match


class _IRIRef(Parser[Tagged]):
    # '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
    def parse(self, input: str) -> ParseResult[Tagged]:
        m = _MATCH_IRI(input)
        if m:
            s = m[1]
            return input[m.end() :], Tagged('IRIREF', s)

        return Error(input)


IRIREF: Parser = _IRIRef()

# LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
LANGTAG: Parser = TaggedParser(
    'LANGTAG',
    pattern_of(
        MatchString(r'@'),
        OneOrMore(RegExp(r'[a-zA-Z]')),
        ZeroOrMore(pattern_of(MatchString(r'-'), OneOrMore(RegExp(r'[a-zA-Z0-9]')))),
    ),
)

_digit_string: Parser = CollectString(OneOrMore(RegExp(r'[0-9]')))

_opt_digit_string: Parser = CollectString(ZeroOrMore(RegExp(r'[0-9]')))

# INTEGER ::= [0-9]+
INTEGER: Parser = TaggedParser('INTEGER', _digit_string)

# DECIMAL ::= [0-9]* '.' [0-9]+
DECIMAL: Parser = TaggedParser(
    'DECIMAL', pattern_of(_opt_digit_string, MatchString(r'.'), _digit_string)
)

# EXPONENT ::= [eE] [+-]? [0-9]+
EXPONENT = TaggedParser(
    'EXPONENT', pattern_of(RegExp(r'[eE]'), Optional(RegExp(r'[+-]')), _digit_string)
)

# DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
DOUBLE: Parser = TaggedParser(
    'DOUBLE',
    either_of(
        pattern_of(_digit_string, MatchString(r'.'), _opt_digit_string, EXPONENT),
        pattern_of(MatchString(r'.'), _digit_string, EXPONENT),
        pattern_of(_digit_string, EXPONENT),
    ),
)

# INTEGER_POSITIVE ::= '+' INTEGER
INTEGER_POSITIVE: Parser = TaggedParser(
    'INTEGER_POSITIVE', Right(MatchString(r'+'), INTEGER)
)

# DECIMAL_POSITIVE ::= '+' DECIMAL
DECIMAL_POSITIVE: Parser = TaggedParser(
    'DECIMAL_POSITIVE', Right(MatchString(r'+'), DECIMAL)
)

# DOUBLE_POSITIVE ::= '+' DOUBLE
DOUBLE_POSITIVE: Parser = TaggedParser(
    'DOUBLE_POSITIVE', Right(MatchString(r'+'), DOUBLE)
)

# INTEGER_NEGATIVE ::= '-' INTEGER
INTEGER_NEGATIVE: Parser = TaggedParser(
    'INTEGER_NEGATIVE', Right(MatchString(r'-'), INTEGER)
)

# DECIMAL_NEGATIVE ::= '-' DECIMAL
DECIMAL_NEGATIVE: Parser = TaggedParser(
    'DECIMAL_NEGATIVE', Right(MatchString(r'-'), DECIMAL)
)

# DOUBLE_NEGATIVE ::= '-' DOUBLE
DOUBLE_NEGATIVE: Parser = TaggedParser(
    'DOUBLE_NEGATIVE', Right(MatchString(r'-'), DOUBLE)
)

# ECHAR ::= '\' [tbnrf\"']
ECHAR: Parser = Right(MatchString('\\'), RegExp(r'[tbnrf\\\"\']'))

# STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
STRING_LITERAL1: Parser = Right(
    MatchString(r"'"),
    Left(
        ReduceToString(
            ZeroOrMore(either_of(RegExp(r'[^\u0027\u005C\u000A\u000D]'), ECHAR))
        ),
        MatchString(r"'"),
    ),
)
# >>> STRING_LITERAL1.parse("'abc'")
# ('', 'abc')

# STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
STRING_LITERAL2: Parser = Right(
    MatchString(r'"'),
    Left(
        ReduceToString(
            ZeroOrMore(either_of(RegExp(r'[^\u0022\u005C\u000A\u000D]'), ECHAR))
        ),
        MatchString(r'"'),
    ),
)
# >>> STRING_LITERAL2().parse('"abc"')
# ('', 'abc')

# STRING_LITERAL_LONG1 ::= "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
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
)

# STRING_LITERAL_LONG2 ::= '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
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
)

# NIL ::= '(' WS* ')'
NIL: Parser = TaggedParser(
    'NIL', pattern_of(MatchString(r'('), CSPACE0, MatchString(r')'))
)

# WS = #x20 | #x9 | #xD | #xA

# ANON ::= '[' WS* ']'
ANON: Parser = TaggedParser(
    'ANON',
    pattern_of(MatchString(r'['), CSPACE0, MatchString(r']')),
)

# PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
PN_CHARS_BASE = Either(RegExp(r'[A-Z]'), RegExp(r'[a-z]'))
# TODO: either_of(RegExp(r'[A-Z]'), RegExp(r'[a-z]'), RegExp(r'[#x00C0-#x00D6]'), RegExp(r'[#x00D8-#x00F6]'), RegExp(r'[#x00F8-#x02FF]'), RegExp(r'[#x0370-#x037D]'), RegExp(r'[#x037F-#x1FFF]'), RegExp(r'[#x200C-#x200D]'), RegExp(r'[#x2070-#x218F]'), RegExp(r'[#x2C00-#x2FEF]'), RegExp(r'[#x3001-#xD7FF]'), RegExp(r'[#xF900-#xFDCF]'), RegExp(r'[#xFDF0-#xFFFD]'), RegExp(r'[#x10000-#xEFFFF]'))

# PN_CHARS_U ::= PN_CHARS_BASE | '_'
PN_CHARS_U = Either(PN_CHARS_BASE, MatchString(r'_'))

# PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
PN_CHARS = either_of(PN_CHARS_U, MatchString(r'-'), RegExp(r'[0-9]'))
# either_of(PN_CHARS_U(), MatchString(r'-'), RegExp(r'[0-9]'), Tagged(name='hexchar', value='x00B7'), RegExp(r'[#x0300-#x036F]'), RegExp(r'[#x203F-#x2040]'))

# PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
PN_PREFIX = ReduceToString(Pair(PN_CHARS_BASE, Optional(ZeroOrMore(PN_CHARS))))
# pattern_of(PN_CHARS_BASE(), Optional(pattern_of(ZeroOrMore(either_of(PN_CHARS(), MatchString(r'.'))), PN_CHARS())))

# PNAME_NS ::= PN_PREFIX? ':'
PNAME_NS: Parser = ReduceToString(Pair(Optional(PN_PREFIX), MatchString(r':')))
# PN_LOCAL_ESC ::= '\' ( '_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%' )
PN_LOCAL_ESC: Parser = RecursiveParser(
    'PN_LOCAL_ESC',
    lambda: CollectString(
        Right(
            MatchString('\\'),
            OneOrMore(
                AnyChar(
                    lambda c: c
                    in (
                        '_',
                        '~',
                        '.',
                        '-',
                        '!',
                        '$',
                        '&',
                        "'",
                        '(',
                        ')',
                        '*',
                        '+',
                        ',',
                        ';',
                        '=',
                        '/',
                        '?',
                        '#',
                        '@',
                        '%',
                    )
                )
            ),
        )
    ),
)

# HEX ::= [0-9] | [A-F] | [a-f]
HEX = either_of(RegExp(r'[0-9]'), RegExp(r'[A-F]'), RegExp(r'[a-f]'))

# PERCENT ::= '%' HEX HEX
PERCENT = Pair(MatchString(r'%'), Pair(HEX, HEX))

# PLX ::= PERCENT | PN_LOCAL_ESC
PLX = Either(PERCENT, PN_LOCAL_ESC)

PN_LOCAL = ReduceToString(
    Pair(
        either_of(PN_CHARS_U, MatchString(r':'), RegExp(r'[0-9]'), PLX),
        Optional(
            pattern_of(
                ZeroOrMore(
                    either_of(PN_CHARS, MatchString(r'.'), MatchString(r':'), PLX)
                )
            )
        ),
    )
)

PNAME_LN: Parser = Pair(PNAME_NS, PN_LOCAL)

# BLANK_NODE_LABEL ::= '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
BLANK_NODE_LABEL: Parser = TaggedParser(
    'BLANK_NODE_LABEL',
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
    ),
)

# VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
VARNAME = ReduceToString(
    Pair(
        Either(PN_CHARS_U, RegExp(r'[0-9]')),
        ZeroOrMore(either_of(PN_CHARS_U, RegExp(r'[0-9]'))),
    )
)
# TODO: pattern_of(either_of(PN_CHARS_U(), RegExp(r'[0-9]')), ZeroOrMore(either_of(PN_CHARS_U(), RegExp(r'[0-9]'), Tagged(name='hexchar', value='x00B7'), RegExp(r'[#x0300-#x036F]'), RegExp(r'[#x203F-#x2040]'))))

# VAR1 ::= '?' VARNAME
VAR1: Parser = TaggedParser('VAR1', Right(MatchString(r'?'), VARNAME))

# VAR2 ::= '$' VARNAME
VAR2: Parser = TaggedParser('VAR2', Right(MatchString(r'$'), VARNAME))


def parse(indata: str):
    parser: Parser = Either(QueryUnit(), UpdateUnit())
    result = parser.parse(indata)
    if isinstance(result, Error):
        raise Exception(result.on)
    next_input, value = result
    if next_input:
        raise Exception(f'Unable to parse: {next_input[:10]!r}')
    return value
