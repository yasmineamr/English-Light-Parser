sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
sentence(s(NP,VP,C,S)) --> noun_phrase(NP), verb_phrase(VP),conjunction(C),sentence(S).
% sentence(s(NP)) --> verb_phrase(NP).
% sentence(s(NP)) --> noun_phrase(NP).

noun_phrase(np(D,N)) --> det(D), noun(N).
noun_phrase(np(D1,N1,C,NP)) --> det(D1), noun(N1), conjunction(C), noun_phrase(NP).

noun_phrase(np(N)) --> noun(N).
noun_phrase(np(N1,C,NP)) --> noun(N1), conjunction(C), noun_phrase(NP).

noun_phrase(np(P)) --> pronoun(P).
noun_phrase(np(P,C,NP)) --> pronoun(P), conjunction(C), noun_phrase(NP).

noun_phrase(np(A,N)) --> adjective_phrase(A), noun(N).
noun_phrase(np(A,N,C,NP)) --> adjective_phrase(A), noun(N), conjunction(C), noun_phrase(NP).

noun_phrase(np(D,A,N)) --> det(D), adjective_phrase(A), noun(N).
noun_phrase(np(D,A,N,C,NP)) --> det(D), adjective_phrase(A), noun(N), conjunction(C), noun_phrase(NP).


noun_phrase(np(D,N,R)) --> det(D), noun(N), relative_clause_phrase(R).
noun_phrase(np(D,N,R,C,NP)) --> det(D), noun(N), relative_clause_phrase(R), conjunction(C), noun_phrase(NP).

noun_phrase(np(D,A,N,R)) --> det(D), adjective_phrase(A), noun(N), relative_clause_phrase(R). %the young boy who worked for the old man
noun_phrase(np(D,A,N,R,C,NP)) --> det(D), adjective_phrase(A), noun(N), relative_clause_phrase(R), conjunction(C), noun_phrase(NP).

noun_phrase(np(D,N,P)) --> det(D), noun(N), preposition_phrase(P).
noun_phrase(np(D,N,P,C,NP)) --> det(D), noun(N), preposition_phrase(P), conjunction(C), noun_phrase(NP).

noun_phrase(np(D,A,N,P)) --> det(D), adjective_phrase(A), noun(N), preposition_phrase(P). % a big box in the room
noun_phrase(np(D,A,N,P,C,NP)) --> det(D), adjective_phrase(A), noun(N), preposition_phrase(P), conjunction(C), noun_phrase(NP).

% noun_phrase(np(Q,N)) --> quantifier(Q), noun(N).
% noun_phrase(np(Q,D,N)) --> quantifier(Q), det(D), noun(N).

% noun_phrase(np(D1,A1,N1,C, D2,A2,N2)) --> det(D1), adjective_phrase(A1), noun(N1), conjunction(C), det(D2), adjective_phrase(A2), noun(N2).
% noun_phrase(np(Q,D,A,N)) --> quantifier(Q), det(D), adjective(A), noun(N).


% noun_phrase(np(N,P)) --> noun_phrase(N), preposition_phrase(P).
% noun_phrase(np(N,P)) --> noun_phrase(N), relative_clause_phrase(P).
% noun_phrase(np(N1,C,N2)) --> noun_phrase(N1), conjunction(C), noun_phrase(N2).

adjective_phrase(adj(A)) --> adjective(adj(A)).
adjective_phrase(adj(A,R)) --> adjective(A), adjective_phrase(R).
%
preposition_phrase(pp(P,N)) --> preposition(P), noun_phrase(N).
preposition_phrase(pp(P)) --> preposition(P).
%
relative_clause_phrase(rcp(R)) --> relative_pronoun(R).
relative_clause_phrase(rcp(R,V)) --> relative_pronoun(R), verb_phrase(V).
relative_clause_phrase(rcp(R,N)) --> relative_pronoun(R), noun_phrase(N).

% adverb_phrase(adv(AD)) --> adverb(AD).
%
verb_phrase(vp(V)) --> verb(V).
verb_phrase(vp(V,C,VP)) --> verb(V), conjunction(C), verb_phrase(VP).

verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
verb_phrase(vp(V,NP,C,VP)) --> verb(V), noun_phrase(NP), conjunction(C), verb_phrase(VP).

verb_phrase(vp(V,PP)) --> verb(V), preposition_phrase(PP).
verb_phrase(vp(V,PP,C,VP)) --> verb(V), preposition_phrase(PP), conjunction(C), verb_phrase(VP).

% verb_phrase(vp(V1,C,V2,PP)) --> verb(V1), conjunction(C), verb(V2), preposition_phrase(PP).

verb_phrase(vp(V,NP1,NP2)) --> verb(V), noun_phrase(NP1), noun_phrase(NP2).

verb_phrase(vp(A,VP)) --> adverb(A), verb_phrase(VP).

% verb_phrase(vp(V1,C,V2,NP)) --> verb(V1), conjunction(C), verb(V2), noun_phrase(NP).
% verb_phrase(vp(V,NP)) --> verb_phrase(V), noun_phrase(NP).

% verb_phrase(vp(V,NP,PP)) --> verb(V), noun_phrase(NP), preposition_phrase(PP).

% s(np(np(d(the), n(boy)), pp(prep(in), np(d(the), adj(blue), n(shirt)))))
pronoun(p(i)) --> [i].
pronoun(p(he)) --> [he].
pronoun(p(she)) --> [she].
pronoun(p(it)) --> [it].
pronoun(p(they)) --> [they].
pronoun(p(we)) --> [we].
pronoun(p(you)) --> [you].
pronoun(p(me)) --> [me].
pronoun(p(our)) --> [our].

conjunction(conj(and)) --> [and].
conjunction(conj(while)) --> [while].

relative_pronoun(rp(who)) --> [who].
relative_pronoun(rp(that)) --> [that].

det(d(the)) --> [the].
det(d(a)) --> [a].
det(d(those)) --> [those].
det(d(every)) --> [every].
det(d(some)) --> [some].
det(d(some)) --> [some].
det(d(any)) --> [any].
det(d(much)) --> [much].
det(d(many)) --> [many].
det(d(all)) --> [all].

noun(n(bat)) --> [bat].
noun(n(cat)) --> [cat].
noun(n(boy)) --> [boy].
noun(n(girl)) --> [girl].
noun(n(people)) --> [people].
noun(n(children)) --> [children].
noun(n(shirt)) --> [shirt].
noun(n(man)) --> [man].
noun(n(woman)) --> [woman].
noun(n(school)) --> [school].
noun(n(room)) --> [room].
noun(n(box)) --> [box].
noun(n(school)) --> [school].
noun(n(shed)) --> [shed].
noun(n(building)) --> [building].
noun(n(envelope)) --> [envelope].
noun(n(tree)) --> [tree].
noun(n(students)) --> [students].
noun(n(professors)) --> [professors].
noun(n(lecturers)) --> [lecturers].
noun(n(scientists)) --> [scientists].
noun(n(researchers)) --> [researchers].

adjective(adj(closest)) --> [closest].
adjective(adj(brilliant)) --> [brilliant].
adjective(adj(new)) --> [new].
adjective(adj(blue)) --> [blue].
adjective(adj(old)) --> [old].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(big)) --> [big].
adjective(adj(young)) --> [young].
adjective(adj(white)) --> [white].
adjective(adj(poor)) --> [poor].
adjective(adj(talented)) --> [talented].
adjective(adj(bright)) --> [bright].
adjective(adj(positive)) --> [positive].
adjective(adj(awful)) --> [awful].
adjective(adj(brave)) --> [brave].
adjective(adj(clever)) --> [clever].
adjective(adj(adorable)) --> [adorable].
adjective(adj(small)) --> [small].
adjective(adj(rich)) --> [rich].

adverb(adv(quickly)) --> [quickly].
adverb(adv(secretly)) --> [secretly].
adverb(adv(carefully)) --> [carefully].
adverb(adv(slowly)) --> [slowly].
adverb(adv(badly)) --> [badly].
adverb(adv(closely)) --> [closely].
adverb(adv(easily)) --> [easily].
adverb(adv(cheerfully)) --> [cheerfully].
adverb(adv(painfully)) --> [painfully].
adverb(adv(happily)) --> [happily].
adverb(adv(bravely)) --> [bravely].
adverb(adv(finally)) --> [finally].
adverb(adv(angirly)) --> [angirly].
adverb(adv(suddenly)) --> [suddenly].
adverb(adv(kindly)) --> [kindly].
adverb(adv(theoretically)) --> [theoretically].
adverb(adv(sadly)) --> [sadly].
adverb(adv(instantly)) --> [instantly].
adverb(adv(lovingly)) --> [lovingly].
adverb(adv(calmly)) --> [calmly].

preposition(prep(in)) --> [in].
preposition(prep(on)) --> [on].
preposition(prep(at)) --> [at].
preposition(prep(for)) --> [for].
preposition(prep(from)) --> [from].
preposition(prep(after)) --> [after].
preposition(prep(before)) --> [before].
preposition(prep(of)) --> [of].
preposition(prep(with)) --> [with].
preposition(prep(behind)) --> [behind].
preposition(prep(below)) --> [below].

verb(v(eats)) --> [eats].
verb(v(played)) --> [played].
verb(v(worked)) --> [worked].
verb(v(pushed)) --> [pushed].
verb(v(stored)) --> [stored].
verb(v(gave)) --> [gave].
verb(v(climbed)) --> [climbed].
verb(v(watched)) --> [watched].
verb(v(admired)) --> [admired].
verb(v(appreciated)) --> [appreciated].
verb(v(added)) --> [added].
verb(v(allowed)) --> [allowed].
verb(v(finished)) --> [finished].
verb(v(entered)) --> [entered].
verb(v(introduced)) --> [introduced].
verb(v(listened)) --> [listened].
verb(v(liked)) --> [liked].
verb(v(chose)) --> [chose].
verb(v(carried)) --> [carried].
verb(v(borrowed)) --> [borrowed].


% ?- sentence(Parse_tree, [the,old,man,and,the,old,woman,gave,the,poor,young,man,a,white,envelope,in,the,shed,behind,the,building], []).
% ?- sentence(Parse_tree, [the,young,boy,who,worked,for,the,old,man,pushed,and,stored,a,big,box,in,the,large,empty,room,after,school], []).
% ?- sentence(Parse_tree, [every,boy,quickly,climbed,some,big,tree,while,every,girl,secretly,watched,some,boy], []).
% ?- sentence(Parse_tree, [some,brilliant,students,and,many,professors,watched,and,admired,talented,lecturers,and,appreciated,bright,scientists,and,researchers], []).


% % at least twenty nouns done
% % at least twenty verbs (with the past tense in action) done
% % at least twenty adjectives done
% % at least ten adverbs done
% % at least ten prepositions done
% % at least ten determiners done
