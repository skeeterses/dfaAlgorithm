with Ada.Text_IO;
with Ada.Characters.Handling;
use Ada.Characters.Handling;

package body dfa is

    package U_Operator_IO is new Ada.Text_IO.Enumeration_IO(U_Operator);
    package B_Operator_IO is new Ada.Text_IO.Enumeration_IO(B_Operator);

--5/14/2020
    --  True/False functions to implement the rules for the recursive
    --  descent parser for regular expressions.
    function nonEmptyExpression(testString : in String) return boolean
    is
    begin
	    return true;
    end nonEmptyExpression;

    function unaryOp(testCharacter : in Character) return boolean is
    begin
	case testCharacter is
	   when '*' =>
	      return true;
	   when '+' =>
	      return true;
	   when '?' =>
	      return true;
	   when others =>
	      return false;
           end case;
    end unaryOp;

    function binaryOp(testCharacter : in Character) return boolean
    is
    begin
	    return true;
    end binaryOp;

--7/29/2020
-- A list of tokenizing functions for the Regular Expression grammar
-- and Extended Regular Expression grammar.

    function ORD_CHAR(testCharacter : in Character) return boolean
    is
       retValue : boolean;
    begin
	retValue := false;
        retValue := Is_Alphanumeric(testCharacter);
	-- Technically, I could use some graphic characters since the regular
	-- expression grammar doesn't reserve all the special graphic 
	-- characters and that a closed parenthesis is only special when 
	-- matched by a preceding open parenthesis.  
	-- If this program works well, I'll implement some of the special 
	-- graphic characters by using the character attributes to get the 
	-- ascii values.

	    return retValue;
    end ORD_CHAR;

    function QUOTED_CHAR(inputString : in String) return boolean is
	    retValue : boolean;
    begin
            retValue := false;
            if inputString'length = 1 then
		if inputString(inputString'first) = '\' then
		    case inputString(inputString'first + 1) is
			when '^' =>
				return true;
			when '.' =>
				return true;
			when '[' =>
				return true;
			when '$' =>
				return true;
			when '(' =>
				return true;
			when ')' =>
				return true;
			when '|' =>
				return true;
			when '*' =>
				return true;
			when '+' =>
				return true;
			when '?' =>
				return true;
			when '{' =>
				return true;
			when '\' =>
				return true;
			when others =>
				return false;
		end case;
		end if;

	    end if;
	    return retValue;
    end QUOTED_CHAR;

    function SPEC_CHAR(testcharacter: in Character) return boolean
    is
    begin
	case testCharacter is
		when '^' => return true;
		when '.' => return true;
		when '[' => return true;
		when '$' => return true;
		when '(' => return true;
		when ')' => return true;
		when '|' => return true;
		when '*' => return true;
		when '+' => return true;
		when '?' => return true;
		when '{' => return true;
		when '\' => return true;
		when others => return false;
	end case;
    end SPEC_CHAR;

    function L_ANCHOR(testcharacter : in Character) return boolean
    is
	    retValue : boolean;
    begin
	retValue := false;
           if testCharacter = '^' then
		   retValue := true;
	   end if;
	return retValue;
    end L_ANCHOR;

-- 7/27/2020
-- For the Regular Expression grammar, I'm using the The Open Group Base
-- Specifications Issue 6, IEEE Std. 1003.1, 2004 Edition.
-- I don't plan to implement {m,n} counting feature but can do that at a
-- later time if the other features work well.
   function basic_reg_exp(inputString : in String) return boolean is
	begin
		return true;
	end basic_reg_exp;

   function RE_expression(inputString : in String) return boolean is
   begin
	   return true;
   end RE_expression;

   function simple_RE(inputString : in String) return boolean is
   begin
	   return true;
   end simple_RE;

   function nondupl_RE(inputString : in String) return boolean is
   begin
	   return true;
   end nondupl_RE;

   function one_char_or_coll_elem_RE(inputString : in String) return boolean is
   begin
	   return true;
   end one_char_or_coll_elem_RE;


--   function RE_dupl_symbol(inputString : in String) return boolean is
--   begin
--	   return true;
--   end RE_dupl_symbol;

--7/27/2020
--   Bracket Expression

   function bracket_expression(inputString : in String) return boolean is
	retValue : boolean;
   	firstIndex, lastIndex : Integer;   
        middleString : String(1..20);
   begin
	retValue := false;
	firstIndex := inputString'first;
	lastIndex := inputString'last;
	if inputString'length >= 3 then
          middleString := inputString(firstIndex+1 .. lastIndex-1);
        end if;

	if (inputString(firstIndex) = '[') and (inputString(lastIndex) = ']')
	then
	 retValue := (matching_List(middleString)) or
	             (nonmatching_List(middleString)); 
	end if;

	return retValue;
   end bracket_expression;

   function matching_list(inputString : in String) return boolean is
   begin
	   return bracket_list(inputString);
   end matching_list;

   function nonmatching_list(inputString : in String) return boolean is
     retValue : boolean;
   begin
	   retValue := false;
	   if inputString(inputString'First) = '^' then
		   return bracket_list(inputString(inputString'First+1 ..
			                           inputString'Last));
	   end if;
	   return retValue;
   end nonmatching_list;

   function bracket_list(inputString : in String) return boolean is    
   begin
      if inputString(inputString'Last) = '-' then
	 return follow_list(
   end bracket_list;

-- 8/4/2020
   -- The grammar for follow_list is
   -- follow_list :: 		   expression_term
   --              | follow_list   expression_term
   function follow_list(inputString : in String) return boolean is
	retValue : boolean;
   begin
	   
   end follow_list;

   function expression_term(inputString : in String) return boolean is
   begin
	   return true;
   end expression_term;

   function single_expression(inputString : in String) return boolean is
   begin
	   return true;
   end single_expression;

   function range_expression(inputString : in String) return boolean is
   begin
	   return true;
   end range_expression;

   function start_range(inputString : in String) return boolean is
   begin
	   return true;
   end start_range;

   function end_range(inputString : in String) return boolean is
   begin
	   return true;
   end end_range;

   function collating_symbol(inputString : in String) return boolean is
   begin
	   return true;
   end collating_symbol;

   function equivalence_class(inputString : in String) return boolean is
   begin
	   return true;
   end equivalence_class;

   function character_class(inputString : in String) return boolean is
   begin
	   return true;
   end character_class;

   
 --7/27/2020
--  Extended Regular Expression

   function extended_reg_exp(inputString : in String) return boolean is
   begin
	   return true;
   end extended_reg_exp;

   function ERE_branch(inputString : in String) return boolean is
   begin
	   return true;
   end ERE_branch;

   function ERE_expression(inputString : in String) return boolean is
   begin
	   return true;
   end ERE_expression;

   function one_char_or_coll_elem_ERE(inputString : in String) return boolean
   is
	retValue : boolean;
   begin
	retValue := false;
	
	if inputString'length = 1 then
           retValue := ORD_CHAR(inputString(inputString'first)) or
	               (inputString(inputString'first) = '.');
	else
		retValue := QUOTED_CHAR(inputString) or
				bracket_expression(inputString);	
	end if;	
	

	   return retValue;
   end one_char_or_coll_elem_ERE;

   function ERE_dupl_symbol(inputString : in String) return boolean is
   	retValue : boolean;
   begin
	   retValue := false;
	   
	   if inputString'length = 1 then
	   retValue := unaryOp(inputString(inputString'first));
	   -- For now, we won't cover the { DUP_COUNT } or
	   -- { DUP_COUNT, DUP_COUNT } part of the regular expression grammar.
           end if;
	   return retValue;
   end ERE_dupl_symbol;


-- 5/14/2020
--   This uses recursive descent parser to check if a string is indeed a
--   regular expression
    function isRegularExpression(inputString : in String) return boolean is
       retValue : boolean;
    begin
        retValue := false;  --Assume that its not a regular expression until
	                    -- its shown that it is.

	return retValue;
    end isRegularExpression;

    procedure PrintNode(inputNode : SyntaxNode) is
    begin
        case inputNode.inputType is
	    when UnaryOperator =>
    	        Ada.Text_IO.Put("Value: ");
		U_Operator_IO.Put(inputNode.U_value);
		Ada.Text_IO.New_Line;
                -- A unary operator MUST have a child node, ex in a*, * is
		-- the operator and 'a' is the child.
		Ada.Text_IO.Put("Child node: ");
		PrintNode(inputNode.SubNode.all);
    
	   when BinaryOperator =>
		Ada.Text_IO.Put("Value: ");
		B_Operator_IO.Put(inputNode.B_value);
		Ada.Text_IO.New_Line;
                --Binary operator has 2 children:  A|B has | as the value,
		--   A as the left child, and B as the right child.
                Ada.Text_IO.Put("Right child of ");
		B_Operator_IO.Put(inputNode.B_value);
		Ada.Text_IO.Put(":");
		PrintNode(inputNode.RightNode.all);
		Ada.Text_IO.New_Line;

                Ada.Text_IO.Put("Left child of ");
		B_Operator_IO.Put(inputNode.B_Value);
		Ada.Text_IO.Put(":");
		PrintNode(inputNode.LeftNode.all);
		Ada.Text_IO.New_Line;

	   when ascii =>
		Ada.Text_IO.Put("Value: ");
		Ada.Text_IO.Put(inputNode.A_value);
		Ada.Text_IO.New_Line;
         end case;	     
    end PrintNode;

end dfa;
