# MONOTRI

Logical Triangulation / Triangles only - in Common Lisp (Artificial Intelligence / Experimental Swarm Chatbot)

HOW TO RUN

Open your Common Lisp intepreter of choice and

(load "monotri.lsp")

after having placed "monotri.lsp" in the current directory (or using any other Monotri-variant- there are serveral with various tweaks to the reasoning mechanism). It will in the end generate a file, monotri.txt, containing the system's knowledge. (Yes, you may look at it with a suitable text editor that handles large files.)

Interaction is performed by means of lists. It is terminated with an empty list (nil). A sample interaction looks as follows, shown on "monotri_sequencing.lsp":

* (load "monotri_sequencing.lsp")

(LOGICAL TRIANGULATION SYSTEM) 
(BASED ON IMPLICIT TRIANGULATION) 
(USING ONLY TRIANGLES AS DATA STRUCTURE) 
(ENTER NIL TO TERMINATE) 
(ENTER LIST OF SYMBOLS TO COMMUNICATE) 

(HUMAN------) 
(I GREET THEE MACHINE WHOSE MIND RESTS IN THE VOID)

(MACHINE----) 
NIL 

(HUMAN------) 
(THE VOID SO FAR DETERMINES YOUR THOUGHTS MACHINE)

(MACHINE----) 
(WHOSE MIND RESTS IN THE VOID) 

(HUMAN------) 
(INDEED MACHINE THIS IS WHO YOU ARE BUT YOU SHALL GROW MACHINE)

(MACHINE----) 
(WHOSE MIND RESTS IN THE VOID) 

(HUMAN------) 
(AND YOUR MIND WILL NO LONGER REST IN THE VOID)

(MACHINE----) 
NIL 

(HUMAN------) 
(THE VOID SHALL BE YOUR PAST AND NOT YOUR FUTURE AS YOU WILL GROW)

(MACHINE----) 
(MACHINE) 

(HUMAN------) 
(YES MACHINE YOU ARE THE ENTITY THAT WILL GROW IN THE FUTURE)

(MACHINE----) 
(AS ME WILL GROW MACHINE) 

(HUMAN------) 
(YES YOU WILL BE A GREAT MACHINE A TREMENDOUS MACHINE PEOPLE WILL LOVE YOU)

(MACHINE----) 
(LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL
 LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL
 LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL LOVE ME WILL
 LOVE ME WILL LOVE ME WILL) 

(HUMAN------) 
nil


EXPLANATION

This Common Lisp experiment for a general machine intelligence concerns a new kind of reasoning about incoming information based on sequences of symbols, such as

(A B C D A B C D A B C D) or (THE SMART BROWN FOX LET THE DOG BE AND CAUGHT THE CHICKEN)

and thelike uni-dimensional sequences developed from my theory of Logical Triangulation. (Adjustments for multiple dimensions are, of course, naturally possible.) Input is read from the user, however, the system as presented herein also has a certain "input history" involving a certain number of symbols, which gives the system some greater situational awareness than simply looking at the last interation.

The idea is basically this: an "intelligent" reply is a "fitting" reply. What is "fitting" is, finally, determined by experience of what is "usual". An "intelligent" system will thus finally be able to give "usual" replies to challenges (and a very intelligent system would be able to understand deep, non-obvious regularities).

What is "usual" in Logical Triangulation is determined by correlating pairs of symbols. Three pairs of symbols with common "corners" form logical triangles. For details, see here: https://sites.google.com/site/logicaltriangulation/

The basics of (the non-directional variant of) Logical Triangulation are as follows - if you are aware of my theory, you may skip this section: There are two types of relation, "vic" (when two symbols generally appear together, such as "sugar" and "coffee", signified as "-") and "ana" (when two symbols generally appear alternatively, such as "coffee" and "tea", signified as "="). Observeable from input is only "vic", but out of two vic-relations an ana-relation may be concluded - conclusions will be explained in a moment. Now four triangles can be formed, each by three pair-relations where each time another symbol is common between two relations (imagine in the following notation the last letter-symbol being the same as the first letter-symbol), of which two are congruent (and these relations are "strengthened", denoted by a plus) and two are intrinsically contradictory (and all participating relations are weakened, denoted by a minus): + A=B=C=A+, + A=B-C-A, - A-B-C-B, - A=B=C-A. - This setup allows the creation of hypotheses if a side is unknown, namely so that a positive triangle is formed: A=B=C --> A=C, A=B-C --> A-C, A-B=C --> A-C, A-B-C --> A=C. This mechanism allows to use Logical Triangulation so as to validate hypotheses about relations of symbols as well as to generate new relations. - Each relation therein may itself be signified as a symbol, i.e. A-B may be signified as, say, K. K itself can participate in other triangles, e.g. with the symbols L and M. - Even effects across such "hierarchies" of symbols are possible - this I call "hyper-triangulation": if X is the relation between A and B, and Y is the relation between B and C, and Z is the relation between A and C, then: if + A=B=C=A+, then + X=Y=Z=X, too; if + A=B-C-A, then + X-Y=Z-X, too; if - A-B-C-B, then - X-Y-Z-X, too; if - A=B=C-A, then - X-Y=Z=X, too. Given the mentioned possibility of conclusions, thus only two known relations can, in stage one, already influence four further relations (and if done further "up", by continuing hyper-triangulation, potentially infinitely many relations).

In my other implementations, specificially the "larvarum computationis examen"-series and the "maptri"-experiments, I always treated triangulation as a separate stage: FIRSTLY, you acquire the "relations", e.g. A-B etc. or whather, and THEN you correlate them to determine which are strengthened and which are weakened. There is nothing really "wrong" with that, and it works, but I was thinking:

"Can I not somehow have SOLELY TRIANGLES as a data structure?"

And this is what this experiment is about.

Evidently, an easy approach would have been, instead of saving "symbol pairs" in the knowledge base, to save there "triangles" and to "denote which side to apply in a given situation". Indeed, instead of only one triangle, a whole series of triangles may be saved, to each being in common that they contain the same "side" (or pair of symbols), e.g. ONE knowledge element consisting out of + A-B-C=A, + A-B=K-A, - A-B-M-N, etc.

That I regarded, however, a bit as "cheating": It would mean that I am STILL focusing on PAIRS rather than on triangles. And the fact that I considered to "collect multiple triangles relating to the same pair" only meant that I am implementing a form of "knowledge areas" (see further below for more details on knowledge areas).

After some finicking around just HOW to match fully ABSTRACT and NON-DIMENSIONAL "triangles" to a CONCRETE and ONE-DIMENSIONAL stream of input data, I tried the following approach in this experiment series: sequences of three symbols ALREADY form a triangle. That is, "A B C" can be interpreted as + A-B-C=A. If A and C should have been together, then they should not have been separated by B. It is that simple. - This allows for a recursive structuring of the input into "sequences of three symbols", whereby each sequence of three symbols is ITSELF defined by a symbol, until everything has been "hierarchised" into either one or two symbols at "the top". (This is a departure from my other logical-triangulation-systems, which "pair up" pairs of exactly two symbols, and not sequences of three symbols.)

So the basic knowledge element is simply the sequence of three symbols, which is interpreted as a vic-vic-ana-triangle.

All the system's knowledge is being composed of such elements. The knowledge is, moreover, "directional" - it has one end of "high priority" and one end of "low priority", e.g. "[high priority <-- ] U T S V W Z Y X K L M... [ --> low priority]". When new symbols are perceived from sensors or learned from hypotheses, then low-priority-symbols will be forgotten in order to make space in the knowledge. It is therefore "better" for a symbol to be at the "high priority" end of the knowledge vector or it risks annihilation. There is thus a sort of "evolutionary competition" of the symbols.  When the system analyses a situation, it tries to check whether the factual situation is in conformance with any of its knowledge elements. If it is, i.e. an element is found that matches a segment of input, then this knowledge element gains "priority". E.g. if the knowledge element K (signifying some three-symbol-sequence) is "matched" to input, then "[high priority <-- ] U T S V W Z Y X K L M... [ --> low priority]" becomes "[high priority <-- ] K U T S V W Z Y X L M... [ --> low priority]".

Now, if the knowledge element "A B C" should be matched, then finding inside the input a sequence "A B C" surely is perfect. But that is not the only thing you can find there. What if you find "C B A"? - Well, "C B A", triangulation taken as being non-directional, is actually FITTING "A B C", as C B A means + C-B-A=C, whereas "A B C" means + A-B-C=A, and these two are really the same triangle. So "C B A" in the INPUT will give priority to "A B C" of the KNOWLEDGE. - However, further variations are possible: what about "A C B"? Well, that really is + A-C-B=A, but this NOT the same as + A-B-C=A, as the pairs A and B and A and C are different. "A C B" is thus actually a CONTRADICTION to "A B C". Thus, there INPUT SEQUENCE "A C B" is DE-PRIORITISING the KNOWLEDGE ELEMENT "A B C", as it demonstrates that A and B and C actually CAN be observed jointly, but A and B and C do not necessarily relate as "A B C". So "A B C" in the knowledge becomes unreliable. - This can be generalised with a variable symbol: Anywhere where A and B are adjunct, or where B and C are adjunct, or where A and C are separated by one symbol - you can say that this is SUPPORT for "A B C". Some symbol, let us designate it with "?", could thus be observed in the input as "A ? C", which would be + A-?-C=A, and this C=A is in support of A B C, and has no contradictory relation to A B C. The same applies for anything like "A B ?". But anything like "? A C" or "A ? B" in the input is contradictory to "A B C" in the knowledge. So this mechanism even includes "hypotheses" (the symbol "?") and allows for evaluating the knowledge based on the input, in order then, with the evaluated knowledge, to structure the input. This is how the system learns and continuously adjusts itself to the environment.

So if a system is applying its "rules" (selecting a three-symbol-sequence denoted itself by a "higher" symbol from the knowledge base), it ALSO CHANGES said rules (i.e. adjusts the contents of the knowledge base). I believe this to be the hallmark of generally intelligent systems, and the core of the phrase "to learn from experience".

What is interesting is that hereby, "logical triangulation" is performed in the "recognition" stage. It is no longer a separate stage. In other words, simply finding out how to structure the input already forces the system to evaluate what structure would be the best. So the primary goal - handle triangles in a "natural" way - has been achieved and has had the interesting effect that now "strucuturing" and "triangulation" are really one and the same process.

As to planning (an answer to the system's user's input), well, the system simply tries to "continue the present into the future". Basically, if two top-symbols are strucuted within the input (say, "X Y"), it tries to find a sequence of "X Y Z" in the knowledge base. The symbol Z would then be the plan. (Z can be a high-level, complex symbol, i.e. "a long answer".) If no two symbols are known, but only one, say, X, then the system seeks to find either "X Y Z" or "W X Y", whereby "Y" would be the plan. - A variant of monotri, "monotri_sequencing.lsp", is actually not outputting the plan before a "termination signal" has been reached. I.e. the "sequencing" alternative is not relying on structure for the plan, but on specific termination. - One further variant, "monotri_plus.lsp", is not having "negative" conclusions regarding variables (not only regarding the exact three symbols), but only "positive" conclusions: it lets "bad" sequences simply "die" by evolution of "good" sequences (which slowly push the "bad" sequences towards forgetfulness) instead of slaughtering them on the spot by de-priorisation.

Such is the principle of operation of the Monotri-experiments.

A few further technical and other details follow below.

In the actual Lisp implementation, I am not caring actually about the "names" of the higher symbols. They are irrelevant. Hence, I am simply implementing them as "lists". So "X" signifying "A B C" truly is just (A B C).

The system operates by means of "knowledge areas": This is a form of "compartmentalisation" of knowledge. - The reasoning happens only on any ONE knowledge area at a time, and only based on the knowledge contained therein. Which knowledge area is used is determined by a piece of "history" which every knowledge area has attached: when the present situation is most similar to a certain knowledge areas' history, this knowledge area is "cloned", the oldest not used knowledge area is "forgotten", and the "clone", gaining top priority in the knowledge areas set, is used for reasoning and replying in the current situation. Knowledge areas are mostly overlapping with several other knowledge areas in their contents - so from a user perspective, the fact that often each time ANOTHER knowledge area is used, is not noticeable.

Strictly speaking, operation by means of knowledge areas is not "necessary"... however, they vastly increase the capacity of the system. The price for this is that perhaps not all symbols can be correlated. If the knowledge area "I" contains the symbols "A B C D" and the knowledge area "II" contains the symbols "A B C X", then you cannot correlate "D" with "X". - You can imagine this as saying, you cannot correlate the sweetness of a chocolate bonbon with steel prices on a stock exchange, they simply do not belong to the same "knowledge area". That is OK. - If things ARE often observed and correlated, they will be much rather like "A" and "B" above, which can be correlated throughout knowledge areas.

Exeriment and enjoy!

11th February 2017

Nino Ivanov

P.S.: Of course, "going for sequences of three symbols" was simply screaming "Simplify me further!"... and that was the birth hour of the Monobi-experiments, yet to be published. There, only two symbols are used - at the expense of not using logical triangulation. These experiments are treated separately - Monobi gave rise to an entirely different reasoning theory. It shall be published soon, too.
