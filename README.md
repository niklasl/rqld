# RQLD

A [SPARQL](https://www.w3.org/TR/sparql11-query/) parser written in Python,
used for representing SPARQL as [JSON-LD](https://www.w3.org/TR/json-ld/).

## Usage
```sh
$ cat <some-sparql-file.rq> | python3 -m rqld.parser
```

## Acknowledgements

The included parser combinators module was originally inspired by an
[article by Bodil Stokke](https://bodil.lol/parser-combinators/), licensed
under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

## License

This code is licensed under the
[ISC license](https://opensource.org/license/isc-license-txt/).
