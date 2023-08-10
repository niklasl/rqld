from .partr.tagging import Tagged, TaggedResultTransformer

from .combinatorsparser import parse
from .terms import *

OPERATORS = {
    '=': 'eq',
    '!=': 'neq',
    '<': 'lt',
    '>': 'gt',
    '<=': 'lte',
    '>=': 'gte',
    'IN': 'in',
    'NOT': 'not',
    '!': 'not',
    '+': 'plus',
    '-': 'minus',
}

PATH_MODS = {
    '?': 'optional',
    '*': 'zeroOrMore',
    '+': 'oneOrMore',
}

BUILTINS = {
    'STR': 'str',
    'LANG': 'lang',
    'LANGMATCHES': 'langMatches',
    'DATATYPE': 'datatype',
    'BOUND': 'bound',
    'IRI': 'iri',
    'URI': 'uri',
    'BNODE': 'bnode',
    'RAND': 'rand',
    'ABS': 'abs',
    'CEIL': 'ceil',
    'FLOOR': 'floor',
    'ROUND': 'round',
    'CONCAT': 'concat',
    'STRLEN': 'strLen',
    'UCASE': 'ucase',
    'LCASE': 'lcase',
    'ENCODE_FOR_URI': 'encode_for_uri',
    'CONTAINS': 'contains',
    'STRSTARTS': 'strStarts',
    'STRENDS': 'strEnds',
    'STRBEFORE': 'strBefore',
    'STRAFTER': 'strAfter',
    'YEAR': 'year',
    'MONTH': 'month',
    'DAY': 'day',
    'HOURS': 'hours',
    'MINUTES': 'minutes',
    'SECONDS': 'seconds',
    'TIMEZONE': 'timezone',
    'TZ': 'tz',
    'NOW': 'now',
    'UUID': 'uuid',
    'STRUUID': 'strUuid',
    'MD5': 'md5',
    'SHA1': 'sha1',
    'SHA256': 'sha256',
    'SHA384': 'sha384',
    'SHA512': 'sha512',
    'COALESCE': 'coalesce',
    'IF': 'if',
    'STRLANG': 'strLang',
    'STRDT': 'strDT',
    'SAMETERM': 'sameTerm',
    'ISIRI': 'isIRI',
    'ISURI': 'isURI',
    'ISBLANK': 'isBlank',
    'ISLITERAL': 'isLiteral',
    'ISNUMERIC': 'isNumeric',
}

AGGREGATES = {
    'COUNT': 'count',
    'SUM': 'sum',
    'MIN': 'min',
    'MAX': 'max',
    'AVG': 'avg',
    'SAMPLE': 'sample',
    'GROUP_CONCAT': 'groupConcat',
}


class SparqlToJsonLdTransformer(TaggedResultTransformer):
    def __init__(self):
        self._counters = {}
        self._ctx = {'rq': RQ_NS}
        self._prefixes = {
            'rdf': RDF_NS,
            'xsd': XSD_NS,
        }

    def _next_blank(self, key: str) -> str:
        if key not in self._counters:
            self._counters[key] = 0
        self._counters[key] += 1
        return f'_:{key}{self._counters[key]}'

    def _pfx(self, pfx):
        ns = self._prefixes[pfx]
        if pfx not in self._ctx:
            self._ctx[pfx] = ns
        return f'{pfx}:'

    def _rdftype(self):
        return f"{self._pfx('rdf')}type"

    def match_Query(self, query):
        ((prologue, querybody), valuesclause) = query
        context = self.match_Prologue(prologue.value)
        querynode = context | self.transform(querybody)
        valuesnode = self.transform(valuesclause)
        if valuesnode is not None:
            querynode['rq:values'] = valuesnode
        return querynode

    def match_Prologue(self, prologue):
        for decl in prologue:
            if decl.name == 'BaseDecl':
                key = BASE
                iriref = decl.value
            else:
                pname_ns, iriref = decl.value
                pfx = self.match_PrefixedName(pname_ns)[ID]
                if pfx in self._ctx:
                    pass  # TODO: handle overwritten prefixes!
                key = VOCAB if pfx == ':' else pfx[:-1]

            self._ctx[key] = iriref.value

        return {CONTEXT: self._ctx}

    def match_SelectQuery(self, select):
        (((select_clause, ds_clauses), where_clause), solution_modifier) = select
        select_node = self.transform(select_clause) | {
            'rq:where': self.transform(where_clause),
            'rq:datasets': self.transform(ds_clauses),
        }
        if solution_modifier.value != (((None, None), None), None):
            select_node |= self.transform(solution_modifier)

        return select_node

    def match_SubSelect(self, select):
        (((select_clause, where_clause), solution_modifier), valuesclause) = select
        select_node = self.transform(select_clause) | {
            'rq:where': self.transform(where_clause),
        }

        if solution_modifier.value != (((None, None), None), None):
            select_node |= self.transform(solution_modifier)

        valuesnode = self.transform(valuesclause)
        if valuesnode is not None:
            querynode['rq:values'] = valuesnode

        return select_node

    def match_SelectClause(self, select_clause):
        modifier, vars = select_clause
        select = (
            {ID: 'rq:wildcard'}
            if vars == '*'
            else {
                LIST: [
                    {'rq:bind': v[0], 'rq:as': v[1]} if isinstance(v, list) else v
                    for v in self.transform(vars)
                ]
            }
        )

        select_key = 'rq:select'
        if modifier is not None:
            select_key += modifier.title()

        return {select_key: select}

    def match_ConstructQuery(self, construct):
        bulk, solution_modifier = construct

        if isinstance(bulk[1], Tagged) and bulk[1].name == 'TriplesTemplate':
            ds_clauses, triples_template = bulk

            wherenode = {
                ID: self._next_blank('where'), GRAPH: self.transform(triples_template)
            }
            construct_node = {
                'rq:constructWhere': wherenode,
            }
        else:
            (construct_template, ds_clauses), where_clause = bulk
            construct_node = {
                'rq:construct': self.transform(construct_template),
                'rq:where': self.transform(where_clause),
            }

        # TODO: handle:
        assert len(ds_clauses) == 0

        if solution_modifier.value != (((None, None), None), None):
            construct_node |= self.transform(solution_modifier)

        return construct_node

    def match_ConstructTriples(self, construct_triples):
        items = self._cons_to_list(construct_triples)

        return {ID: self._next_blank('construct'), GRAPH: items}

    def match_Update(self, update):
        results = []
        while update is not None:
            (prologue, (update1, update)) = update
            ctx = self.match_Prologue(prologue.value)
            results.append(ctx | {GRAPH: self.transform(update1)})

        return results[0] if len(results) == 1 else results

    def match_Modify(self, modify):
        (((with_iri, delins_or_ins), using_clauses), where_pattern) = modify
        modify_node = {}
        if with_iri is not None:
            modify_node['rq:with'] = with_iri

        if isinstance(delins_or_ins, Tagged):
            modify_node |= self.transform(delins_or_ins)
        else:
            delete_clause, opt_insert_clause = delins_or_ins
            modify_node |= self.transform(delete_clause)
            if opt_insert_clause is not None:
                modify_node |= self.transform(opt_insert_clause)

        modify_node['rq:where'] = self.transform(where_pattern)

        return modify_node

    def match_DeleteClause(self, delete_clause):
        return {'rq:delete': self.transform(delete_clause)}

    def match_InsertClause(self, insert_clause):
        return {'rq:insert': self.transform(insert_clause)}

    def match_SolutionModifier(self, solution_modifier):
        (((group_clause, having_clause), order_clause), limit_offset) = solution_modifier
        solution_node = {}

        if group_clause is not None:
            solution_node['rq:groupBy'] = {LIST: self.transform(group_clause)}

        if having_clause is not None:
            solution_node['rq:having'] = {LIST: self.transform(having_clause)}

        if order_clause is not None:
            solution_node['rq:orderBy'] = {LIST: self.transform(order_clause)}

        if limit_offset is not None:
            one, opt_other = limit_offset
            solution_node |= self.transform(one)
            if opt_other:
                solution_node |= self.transform(opt_other)

        return solution_node

    def match_GroupCondition(self, condition):
        if isinstance(condition, tuple) and not isinstance(condition, Tagged):
            expr, name = condition
            return {'rq:bind': self.transform(expr), 'rq:as': self.transform(name)}
        else:
            return self.transform(condition)

    def match_OrderCondition(self, order):
        if isinstance(order, tuple) and order[0] in {'DESC', 'ASC'}:
            return {f'rq:{order[0].lower()}': self.transform(order[1])}
        else:
            return self.transform(order)

    def match_LimitClause(self, limit):
        return {'rq:limit': self.transform(limit)}

    def match_OffsetClause(self, offset):
        return {'rq:offset': self.transform(offset)}

    def match_Quads(self, quads):
        quad_sets = self._cons_to_list(quads)
        return {
            ID: self._next_blank('quads'),
            GRAPH: quad_sets[0] if len(quad_sets) == 1 else quad_sets,
        }

    def match_GroupOrUnionGraphPattern(self, group_or_union_graph_pattern):
        return {
            'rq:union': {LIST: self._first_more_to_list(group_or_union_graph_pattern)}
        }

    def match_TriplesTemplate(self, triples_template):
        return self._wrapped_cons_to_list(triples_template)

    def match_TriplesSameSubject(self, node_triples):
        # TODO: either this or TriplesNode, PropertyList
        subject, property_list = node_triples

        node = self.transform(subject)
        if ID in node and node[ID] is None:
            del node[ID]

        self._populate_node(node, property_list.value)

        return node

    def match_BlankNodePropertyList(self, property_list):
        node = {}

        self._populate_node(node, property_list.value)

        return node

    def _populate_node(self, node, property_list) -> None:
        first, items = property_list
        items.insert(0, first)
        for pair in items:
            verb, object_list = pair
            p = self.transform(verb)
            objects = self.transform(object_list)
            if p == TYPE or p[ID] == 'rdf:type':
                node[TYPE] = [o[ID] for o in objects]
            else:
                node[p[ID]] = objects

    def match_GroupGraphPattern(self, graph_pattern):
        return {ID: self._next_blank('where'), GRAPH: self.transform(graph_pattern)}

    def match_GroupGraphPatternSub(self, graph_pattern_sub):
        (triples_block, more_blocks) = graph_pattern_sub
        items = [triples_block]
        for not_triples, next_triples_block in more_blocks:
            items.append(not_triples)
            items.append(next_triples_block)

        result = []
        for it in items:
            o = self.transform(it)
            if isinstance(o, list):
                result += o
            elif o is not None:
                result.append(o)

        return result

    def match_TriplesBlock(self, triples_block):
        return self._wrapped_cons_to_list(triples_block)

    def match_TriplesSameSubjectPath(self, node_triples):
        # TODO: either this or TriplesNodePath, PropertyListPath
        var_or_term, property_list_path = node_triples

        node = self.transform(var_or_term)
        if node[ID] is None:
            del node[ID]

        self._add_property_list_path(node, property_list_path)

        return node

    def match_BlankNodePropertyListPath(self, property_list_path):
        node = {}
        self._add_property_list_path(node, property_list_path)
        return node

    def _add_property_list_path(self, node, property_list_path) -> None:
        (some_verb, object_list_path), items = property_list_path.value
        items.insert(0, (some_verb, object_list_path))
        for some_verb, object_list_path in items:
            p = self.transform(some_verb)
            objects = self.transform(object_list_path)

            if p == 'a' or p == TYPE or p.get(ID) == 'rdf:type':
                if all(ID in o for o in objects):
                    node[TYPE] = [o[ID] for o in objects]
                else:
                    node[self._rdftype()] = objects

            elif ID in p and isinstance(p[ID], str):
                p_key = p[ID]
                node.setdefault(p_key, []).extend(objects)
            else:
                node.setdefault('rq:statement', []).append(
                    {'rq:path': {LIST: p}, 'rq:object': objects}
                )

    def match_PathAlternative(self, path_alt):
        return self._combined_parts(path_alt, 'rq:alt')

    def match_PathSequence(self, path_alt):
        return self._combined_parts(path_alt, 'rq:seq')

    def _combined_parts(self, first_and_rest, op: str):
        parts = self._first_more_to_list(first_and_rest)
        return {op: {LIST: parts}} if len(parts) > 1 else parts[0]

    def match_PathEltOrInverse(self, path_elt_or_inverse):
        if path_elt_or_inverse[0] == '^':
            return {'rq:inverse': self.transform(path_elt_or_inverse[1])}
        else:
            return self.transform(path_elt_or_inverse)

    def match_PathElt(self, path_elt):
        path_prim, path_mod = path_elt
        path_obj = self.transform(path_prim)
        if path_mod is not None:
            mod_op = PATH_MODS[path_mod]
            return {f'rq:{mod_op}': path_obj}
        else:
            return path_obj

    def match_PathPrimary(self, path_prim):
        value = self.transform(path_prim)
        if value == 'a':
            return {ID: self._rdftype()}
        return value

    def match_PathNegatedPropertySet(self, neg_propset):
        # TODO: also handle rest sequence
        return {'rq:negated': self.transform(neg_propset)}

    def match_PathOneInPropertySet(self, neg_propset):
        if neg_propset[0] == '^':
            return {'rq:inverse': self.transform(neg_propset[1])}
        value = self.transform(neg_propset)
        if value == 'a':
            return {ID: 'rdf:type'}
        return value

    def match_ObjectListPath(self, object_list_path):
        return self._first_more_to_list(object_list_path)

    def match_OptionalGraphPattern(self, opt_graph_pattern):
        return {'rq:optional': self.transform(opt_graph_pattern)}

    def match_GraphGraphPattern(self, named_graph_pattern):
        named, graph_pattern = named_graph_pattern
        return {
            ID: self.transform(named)[ID],
            'rq:where': self.transform(graph_pattern),
        }

    def match_ServiceGraphPattern(self, service_graph_pattern):
        (silent, service), graph_pattern = service_graph_pattern
        return {
            'rq:service': self.transform(service),
            'rq:silent': True if silent is not None else None,
            'rq:where': self.transform(graph_pattern),
        }

    def match_Bind(self, bind):
        bound_expr, as_expr = bind
        return {
            ID: self.transform(as_expr)[ID],
            'rq:boundTo': self.transform(bound_expr),
        }

    def match_InlineData(self, inline_data):
        return {
            'rq:values': self.transform(inline_data),
        }

    def match_InlineDataOneVar(self, var_items):
        v, items = var_items
        return {
            'rq:var': self.transform(v),
            'rq:rows': {LIST: [self._inlinevalue(it) for it in items]},
        }

    def match_InlineDataFull(self, vars_rows):
        vars, rows = vars_rows
        return {
            'rq:vars': {LIST: [self.transform(x) for x in vars]},
            'rq:rows': {
                LIST: [{LIST: [self._inlinevalue(x) for x in row]} for row in rows]
            },
        }

    def _inlinevalue(self, v):
        return (
            {ID: "rq:undef"}
            if isinstance(v, str) and v.upper() == "UNDEF"
            else self.transform(v)
        )

    def match_BuiltInCall(self, call) -> dict[str, object]:
        func, *args = call
        while not isinstance(func, str):
            func, firstarg = func
            args.insert(0, firstarg)

        call_prop: str
        if func == 'ExistsFunc':
            call_prop = f'rq:exists'
        elif func == 'NotExistsFunc':
            call_prop = f'rq:notExists'
        elif func in {
            'Aggregate',
            'SubstringExpression',
            'StrReplaceExpression',
            'RegexExpression',
        }:
            return self.transform(call)
        elif func.upper() == 'IF':
            cond, then, otherwise = args
            return {
                'rq:if': self.transform(cond),
                'rq:then': self.transform(then),
                'rq:else': self.transform(otherwise),
            }
        else:
            call_prop = f'rq:{BUILTINS[func.upper()]}'

        args_node = self.transform(args)
        if isinstance(args_node, list) and len(args_node) > 1:
            args_node = {LIST: args_node}

        return {call_prop: args_node}

    def match_Aggregate(self, aggregate):
        func, (distinct, expr) = aggregate

        separator = None
        if func == 'GROUP_CONCAT':
            _, separator = expr
            distinct, expr = distinct

        func_name = AGGREGATES[func]
        call_prop = f'rq:{func_name}'
        aggr_node = {
            call_prop: self.transform(expr),
            'rq:distinct': True if distinct is not None else None,
        }

        if separator is not None:
            aggr_node['rq:separator'] = separator

        return aggr_node

    def match_ExpressionList(self, exprs):
        return {LIST: self._first_more_to_list(exprs)}

    def match_NIL(self, exprs):
        return {LIST: []}

    def match_Filter(self, obj):
        return {'rq:filter': self.transform(obj)}

    def match_ConditionalOrExpression(self, cons: tuple):
        return self._conditional_or_first(cons, 'rq:or')

    def match_ConditionalAndExpression(self, cons: tuple):
        return self._conditional_or_first(cons, 'rq:and')

    def _conditional_or_first(self, cons, op: str):
        first, rest = cons
        first = self.match(first)
        if len(rest) > 0:
            value = [first] + [self.transform(x) for x in rest]
            return {op: {LIST: value}}
        else:
            return first

    def match_RelationalExpression(self, expr):
        lhs, op_rhs = expr
        if op_rhs is None:
            return self.transform(lhs)
        else:
            op, rhs = op_rhs
            if isinstance(op, tuple) and op == ('NOT', 'IN'):
                op_name = 'notIn'
            else:
                op_name = OPERATORS[op]
            return {f'rq:{op_name}': {LIST: [self.transform(lhs), self.transform(rhs)]}}

    def match_AdditiveExpression(self, expr):
        return self._binary_expr(':q:add', expr)

    def match_MultiplicativeExpression(self, expr):
        return self._binary_expr(':q:mul', expr)

    def _binary_expr(self, op, expr):
        lhs, rhs_list = expr
        if len(rhs_list) == 0:
            return self.transform(lhs)
        return {op: self.transform(expr)}

    def match_UnaryExpression(self, expr):
        if isinstance(expr, Tagged):
            return self.transform(expr)
        else:
            un_op, rhs = expr
            op_key = OPERATORS[un_op]
            return {op_key: self.transform(rhs)}

    def match_SubstringExpression(self, expr):
        ((strexpr, strstart), opt_end) = expr
        args = [self.transform(strexpr), self.transform(strstart)]
        if opt_end is not None:
            args.append(self.transform(opt_end))
        return {'rq:substr': {LIST: args}}

    def match_StrReplaceExpression(self, expr):
        (((strexpr, match), replacement), flags) = expr
        args = [
            self.transform(strexpr),
            self.transform(match),
            self.transform(replacement),
        ]
        if flags is not None:
            args.append(self.transform(flags))
        return {'rq:replace': {LIST: args}}

    def match_RegexExpression(self, regexpr):
        ((expr, regex), flags) = regexpr
        args = [self.transform(expr), self.transform(regex)]
        if flags is not None:
            args.append(self.transform(flags))
        return {'rq:regex': {LIST: args}}

    def match_RDFLiteral(self, literal):
        tagged_value, langtag_or_datatype = literal
        value = self.transform(tagged_value)
        if langtag_or_datatype is not None:
            literal_node = {VALUE: value}
            if isinstance(langtag_or_datatype, str):
                literal_node[TYPE] = langtag_or_datatype
            else:
                literal_node[LANGUAGE] = self.transform(langtag_or_datatype)
        else:
            return value

    def match_Verb(self, value):
        if value == 'a':
            return TYPE
        else:
            return self.transform(value)

    def match_ObjectList(self, object_list):
        return self._first_more_to_list(object_list)

    def match_Collection(self, coll):
        return {LIST: self.transform(coll)}

    def match_CollectionPath(self, coll):
        return {LIST: self.transform(coll)}

    def match_PrefixedName(self, pname):
        if isinstance(pname, tuple):
            pname = pname[1] if pname[0] == ':' else ''.join(pname)
        return {ID: f"{pname}"}

    def match_ANON(self, _):
        return {}

    def match_BLANK_NODE_LABEL(self, blank_id: str):
        return {ID: f'_:{blank_id}'}

    def match_VAR1(self, var: str):
        return {ID: f'var:{var}'}

    match_VAR2 = match_VAR1

    def match_IRIREF(self, iriref: str):
        return {ID: iriref}

    def match_INTEGER(self, s: str):
        return int(s)

    def match_INTEGER_POSITIVE(self, integer):
        return {TYPE: f"{self._pfx('xsd')}integer", VALUE: '+' + integer.value}

    def match_INTEGER_NEGATIVE(self, integer):
        return -self.transform(integer)

    def match_DECIMAL(self, parts):
        ((whole, dot), fraction) = parts
        return float(f'{whole}.{fraction}')

    def match_DECIMAL_POSITIVE(self, decimal):
        return {
            TYPE: f"{self._pfx('xsd')}decimal",
            VALUE: '+' + str(self.transform(decimal)),
        }

    def match_DECIMAL_NEGATIVE(self, decimal):
        return -self.transform(decimal)

    def match_DOUBLE(self, double):
        (decimal, exponent) = double

        match decimal:
            case ((whole, dot), fraction):
                s = f'{whole}.{fraction}'
            case (dot, fraction):
                s = f'.{fraction}'
            case str:
                s = decimal

        ((exp, exp_sign), exp_value) = exponent.value
        if exp_sign is None:
            exp_sign = ''

        return {
            TYPE: f"{self._pfx('xsd')}double",
            VALUE: f'{s}{exp}{exp_sign}{exp_value}',
        }

    def match_DOUBLE_POSITIVE(self, double):
        double_node = self.transform(double)
        double_node[VALUE] = '+' + double_node[VALUE]
        return double_node

    def match_DOUBLE_NEGATIVE(self, double):
        double_node = self.transform(double)
        double_node[VALUE] = '-' + double_node[VALUE]
        return double_node

    def match_BooleanLiteral(self, booltext: str):
        return booltext == 'true'


if __name__ == '__main__':
    import json
    import sys

    def eprint(*args):
        print(*args, file=sys.stderr)

    indata = sys.stdin.read()
    root = parse(indata)

    transformer = SparqlToJsonLdTransformer()
    result = transformer.transform(root)

    print(json.dumps(result, indent=2))
