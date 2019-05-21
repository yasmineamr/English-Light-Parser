# English-Light-Parser
A parser for a definite clause grammar of English-Light in Prolog.

The parser results in building a parse tree for the input.

A parse tree is represented as follows:
* l, where l is the label of a leaf.
* p(l1,l2,..,ln), where p is a label of a parent node and li is the ith sub-tree thereof, where left-to-right order is assumed.

Run the queries in the following format:
```
sentence(Parse_tree, [the,boy,pushed,a,box], []).
```
