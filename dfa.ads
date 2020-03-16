--  Dfa.ads
--  3/13/2020
--  Scott Stinson
--  This file will contain procedures and functions inside the Dfa package
--    for reading regular expressions and then creating DFAs and then
--    from the DFAs, creating FSM transition tables in C.

with ada.Unchecked_Deallocation;

package dfa is
        type SyntaxNode;
	type SyntaxNodeType is (UnaryOperator, BinaryOperator, ascii);
	type U_Operator is (star, plus, question);
	type B_Operator is (or_symbol, dot);

	type SyntaxNodePtr is access SyntaxNode;

	--Syntax tree for regular expressions
     	type SyntaxNode(inputType : SyntaxNodeType := ascii) is record
		case inputType is
		  when UnaryOperator =>
			  U_Value : U_Operator;
			  SubNode : SyntaxNodePtr;
		  when BinaryOperator =>
			  B_Value : B_Operator;
			  LeftNode : SyntaxNodePtr;
			  RightNode : SyntaxNodePtr;
		  when ascii =>
			  A_Value : Character;
	          end case;
	end record;

	procedure dispose is new Ada.Unchecked_Deallocation
	    (Object => SyntaxNode, Name => SyntaxNodePtr);

	procedure PrintNode(inputNode : SyntaxNode);

end dfa;

