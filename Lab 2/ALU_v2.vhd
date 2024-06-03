library lab2;
use lab2.bv_arithmetic.all;
use lab2.dlx_types.all;

entity alu is 
	generic(prop_delay: Time := 5 ns);
	port(operand1, operand2: in dlx_word; operation: in alu_operation_code; 
		signed: in bit; result: out dlx_word; error: out error_code);
end entity alu;

architecture behaviour1 of alu is
begin 
	ALUoperation : process(operand1,operand2,operation,signed) is 
		variable overflow : boolean := false;
		variable op_result : dlx_word;
		variable result_bool : boolean := false;
		variable op1 : dlx_word;
		variable op2 : dlx_word;
	begin 
		error <= "0000";
		if operation = x"0" then
		-- ADD,ADDU,ADDI,ADDIU
		   if signed = '1' then
		   	bv_neg(operand1,op1,overflow);
			bv_neg(operand2,op2,overflow);
			if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
			end if;
			bv_addu(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		  	else 
				result <= op_result;
		  	end if;
		   elsif signed = '0' then
			bv_addu(operand1,operand2,op_result,overflow);
		   	if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		   	else 
				result <= op_result;
		  	end if;
		   else 
			error <= "0001" after prop_delay;
			result <= x"00000000" after prop_delay;
		   end if;
		  

		elsif operation = x"1" then
		-- SUB,SUBU,SUBI,SUBIU
		   if signed = '1' then --signed
			bv_neg(operand1,op1,overflow);
			bv_neg(operand2,op2,overflow);
			if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
			end if;
		   	bv_sub(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		   	else 
				result <= op_result;
		   	end if;
		   elsif signed = '0' then --unsigned
			bv_subu(operand1,operand2,op_result,overflow);
			if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		   	else 
				result <= op_result;
		   	end if;
		   else 
			error <= "0001" after prop_delay;
			result <= x"00000000" after prop_delay;
		   end if;

		elsif operation = x"2" then
		-- AND,ANDI
		     result <= operand1 AND operand2 after prop_delay;

		elsif operation = x"3" then
		-- OR
		    result <= operand1 or operand2 after prop_delay;

		elsif operation = x"e" then
		-- MUL,MULU
		   if signed = '1' then --signed
			bv_neg(operand1,op1,overflow);
			bv_neg(operand2,op2,overflow);
			if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
			end if;
		   	bv_mult(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		   	else 
				result <= op_result;
			end if;
		   elsif signed = '0' then --unsigned
			bv_multu(operand1,operand2,op_result,overflow);
			if overflow then
				error <= "0001" after prop_delay;
				result <= x"00000000" after prop_delay;
		   	else 
				result <= op_result;
			end if;
		   else 
			error <= "0001" after prop_delay;
			result <= x"00000000" after prop_delay;
		   end if;

		elsif operation = x"b" then
		-- STL
		    result_bool := bv_lt(operand1,operand2);
		    if result_bool then
			result <= x"00000001" after prop_delay;
		    else 
			result <= x"00000000" after prop_delay;
		    end if;

		elsif operation = x"4" then
		-- XOR, not used
		    result <= x"00000000";

		elsif operation = x"5" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"6" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"7" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"8" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"9" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"a" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"c" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"d" then
		-- unused
		    result <= x"00000000";
		elsif operation = x"f" then
		-- unused
		    result <= x"00000000";
		else
		    error <= "0001" after prop_delay;
		    result <= x"00000000" after prop_delay;
		end if;
	end process ALUoperation;
end architecture behaviour1;