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

        function isRegularExpression(inputString : in String) return boolean;

	--The input for this will most likely have to be a linked list
	--containing SyntaxNode
	--function createDFA(inputString : in String) return SyntaxNodePtr;

        --The FSM itself will be a 'C' sourcefile containing a list
	--of return state names and the transition tables.
	--procedure createFSM(inputSyntaxNode : in SyntaxNodePtr;
	--	            FSMType : Integer);
	

	private
	function matching_list(inputString : in String) return boolean;
	function nonmatching_list(inputString : in String) return boolean;
        function nonEmptyExpression(testString : in String) return boolean;
	function unaryOp(testCharacter : in Character) return boolean;
        function binaryOp(testCharacter : in Character) return boolean;
        function ORD_CHAR(testCharacter : in Character) return boolean;
        function QUOTED_CHAR(inputString : in String) return boolean;
        function SPEC_CHAR(testcharacter: in Character) return boolean;
        function L_ANCHOR(testcharacter : in Character) return boolean;
        function basic_reg_exp(inputString : in String) return boolean;
        function RE_expression(inputString : in String) return boolean;
        function simple_RE(inputString : in String) return boolean;
        function nondupl_RE(inputString : in String) return boolean;
  function one_char_or_coll_elem_RE(inputString : in String) return boolean;
  function bracket_expression(inputString : in String) return boolean;
  function bracket_list(inputString : in String) return boolean;
  function follow_list(inputString : in String) return boolean;
  function expression_term(inputString : in String) return boolean;
  function single_expression(inputString : in String) return boolean;
  function range_expression(inputString : in String) return boolean;
  function start_range(inputString : in String) return boolean;
  function end_range(inputString : in String) return boolean;
  function collating_symbol(inputString : in String) return boolean;
  function equivalence_class(inputString : in String) return boolean;
  function character_class(inputString : in String) return boolean;
  function extended_reg_exp(inputString : in String) return boolean;
  function ERE_branch(inputString : in String) return boolean;
  function ERE_expression(inputString : in String) return boolean;
  function one_char_or_coll_elem_ERE(inputString : in String) return boolean;
  function ERE_dupl_symbol(inputString : in String) return boolean;

end dfa;

