with Ada.Text_IO;
--The Text_IO and the new Enumeration_IO package will be used for debugging
--  the tree
--
--  Dfa.ads
--  3/13/2020
--  Scott Stinson
--  This file will contain procedures and functions inside the Dfa package
--    for reading regular expressions and then creating DFAs and then
--    from the DFAs, creating FSM transition tables in C.

package dfa is
        type SyntaxNodeType is (UnaryOperator, BinaryOperator, ascii);
	type U_Operator is (star, plus, question);
	type B_Operator is (or_symbol, dot);

	type SyntaxNodePtr is access SyntaxNode;

	--Syntax tree for regular expressions
     	type SyntaxNode(inputType : SyntaxNodeType := ascii) is record
		case inputType is
		  when UnaryOperator =>
			  Value : U_Operator;
			  SubNode : SyntaxNodePtr;
		  when BinaryOperator =>
			  Value : B_Operator;
			  LeftNode : SyntaxNodePtr;
			  RightNode : SyntaxNodePtr;
		  when ascii =>
			  Value : Character;
	          end case;
	end record;


end dfa;

