# ELIZA

This is a reproduction of the original ELIZA program, a simulated psychotherapy conversation. Much, or arguably all, of the power in this program merely comes from pattern matching with custom built matching engines (not through regex, essentially). 

So far, the program supports the following funtionalities, given of course the basic premise of a discussion with a therapist:
- Memory mechanism 
  - Function: Recalls previous inputs when input matches no pattern
  - Implementation: Pushing all inputs, except for "hello", into a master input list, then randomly selecting one and pushing it through the "apply-pattern" function when the input is unknown
- Synonym Recognition
  - Function: Removes distinctions between synonyms when pattern matching.
  - Implementation: For each word, checks if synonyms exist; if both have synonyms check if their synonyms are identical.

DISCLAIMER: This program is badly commented, so I've compiled some key functions to look out for:
- pat-match
  - Arguably the most important function in the entire program. Matches a pattern against an input, and returns a) the bindings of the variable and the word that matches, and b) return whether or not there is a match.
- segment-match
  - Matching wherein a variable matches a segment of the input rather than just one word.
- syn-eql 
  - Comparison of any objects; if there are synonyms, matches those instead. 
- eliza
  - Basic read, eval, print loop.
- use-eliza-rules
  - Applies patterns on input. (The eval stage.)
