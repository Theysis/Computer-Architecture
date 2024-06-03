-- dlx_datapath.vhd

package dlx_types is
  subtype dlx_word is bit_vector(31 downto 0); 
  subtype half_word is bit_vector(15 downto 0); 
  subtype byte is bit_vector(7 downto 0); 

  subtype alu_operation_code is bit_vector(3 downto 0); 
  subtype error_code is bit_vector(3 downto 0); 
  subtype register_index is bit_vector(4 downto 0);

  subtype opcode_type is bit_vector(5 downto 0);
  subtype offset26 is bit_vector(25 downto 0);
  subtype func_code is bit_vector(5 downto 0);
end package dlx_types; 

use work.dlx_types.all; 
use work.bv_arithmetic.all;  

entity alu is 
     port(operand1, operand2: in dlx_word; operation: in alu_operation_code; 
          signed: in bit; result: out dlx_word; error: out error_code); 
end entity alu; 

architecture behaviour of alu is
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
				error <= "0001";
				result <= x"00000000";
			end if;
			bv_addu(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001";
				result <= x"00000000";
		  	else 
				result <= op_result;
		  	end if;
		   elsif signed = '0' then
			bv_addu(operand1,operand2,op_result,overflow);
		   	if overflow then
				error <= "0001";
				result <= x"00000000";
		   	else 
				result <= op_result;
		  	end if;
		   else 
			error <= "0001";
			result <= x"00000000";
		   end if;
		  

		elsif operation = x"1" then
		-- SUB,SUBU,SUBI,SUBIU
		   if signed = '1' then --signed
			bv_neg(operand1,op1,overflow);
			bv_neg(operand2,op2,overflow);
			if overflow then
				error <= "0001";
				result <= x"00000000";
			end if;
		   	bv_sub(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001";
				result <= x"00000000";
		   	else 
				result <= op_result;
		   	end if;
		   elsif signed = '0' then --unsigned
			bv_subu(operand1,operand2,op_result,overflow);
			if overflow then
				error <= "0001";
				result <= x"00000000";
		   	else 
				result <= op_result;
		   	end if;
		   else 
			error <= "0001";
			result <= x"00000000";
		   end if;

		elsif operation = x"2" then
		-- AND,ANDI
		     result <= operand1 AND operand2;

		elsif operation = x"3" then
		-- OR
		    result <= operand1 or operand2;

		elsif operation = x"e" then
		-- MUL,MULU
		   if signed = '1' then --signed
			bv_neg(operand1,op1,overflow);
			bv_neg(operand2,op2,overflow);
			if overflow then
				error <= "0001";
				result <= x"00000000";
			end if;
		   	bv_mult(op1,op2,op_result,overflow);
		   	if overflow then
				error <= "0001" ;
				result <= x"00000000";
		   	else 
				result <= op_result;
			end if;
		   elsif signed = '0' then --unsigned
			bv_multu(operand1,operand2,op_result,overflow);
			if overflow then
				error <= "0001" ;
				result <= x"00000000";
			else 
				result <= op_result;
			end if;
		   else 
			error <= "0001";
			result <= x"00000000";
		   end if;

		elsif operation = x"b" then
		-- STL
		    result_bool := bv_lt(operand1,operand2);
		    if result_bool then
			result <= x"00000001";
		    else 
			result <= x"00000000";
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
		    error <= "0001";
		    result <= x"00000000";
		end if;
	end process ALUoperation;
end architecture behaviour;


use work.dlx_types.all; 

entity mips_zero is
  
  port (
    input  : in  dlx_word;
    output : out bit);

end mips_zero;

architecture behaviour of mips_zero is 
begin 
	zero: process(input) is
	begin
	output <= '0';
	end process zero;
end architecture behaviour;

use work.dlx_types.all; 

entity mips_register is
     port(in_val: in dlx_word; clock: in bit; out_val: out dlx_word);
end entity mips_register;

architecture behaviour of mips_register is 
begin
	bitregister : process(in_val,clock) is 
	begin
		if clock = '1' then 
			out_val <= in_val;
		end if;
	end process bitregister;	
end architecture behaviour;


use work.dlx_types.all; 

entity mips_bit_register is
     port(in_val: in bit; clock: in bit; out_val: out bit);
end entity mips_bit_register;

architecture behaviour of mips_bit_register is 
begin
	bitreg : process(in_val,clock) is 
	begin
		if clock = '1' then 
			out_val <= in_val;
		end if;
	end process bitreg;	
end architecture behaviour;

use work.dlx_types.all; 

entity mux is
     port (input_1,input_0 : in dlx_word; which: in bit; output: out dlx_word);
end entity mux;

architecture behaviour of mux is 
begin
	two_way_multi : process(which,input_1,input_0) is
	begin 
		if which = '1' then 
			output <= input_1;
		else 
			output <= input_0;
		end if;
	end process two_way_multi;
end architecture behaviour; 


use work.dlx_types.all;

entity index_mux is
     port (input_1,input_0 : in register_index; which: in bit; output: out register_index);
end entity index_mux;

architecture behaviour of index_mux is 
begin
	five_way_multi : process(which,input_1,input_0) is
	begin 
		if which = '1' then 
			output <= input_1;
		else 
			output <= input_0;
		end if;
	end process five_way_multi;
end architecture behaviour; 


use work.dlx_types.all;

entity sign_extend is
     port (input: in half_word; signed: in bit; output: out dlx_word);
end entity sign_extend;

architecture behaviour of sign_extend is 
begin
	sig_extend : process(input) is 
	begin
		output <= (others => input(15));
		output(15 downto 0) <= input;
	end process sig_extend;
end architecture behaviour;

use work.dlx_types.all; 
use work.bv_arithmetic.all; 

entity add4 is
    port (input: in dlx_word; output: out dlx_word);
end entity add4;

architecture behaviour of add4 is 
begin
	pc_inc : process(input) is
	variable four : dlx_word;
	variable tempInput : dlx_word;
	variable tempResult : dlx_word;
	variable overflow : boolean;
	begin 
		four := X"00000004";
		bv_addu(input,four,tempResult,overflow);
		if overflow then 
			bv_neg(input,tempInput,overflow);
			bv_sub(four,tempInput,tempResult,overflow);
		end if;
		output <= tempResult;
	end process pc_inc;
end architecture behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;  

entity regfile is
     port (read_notwrite,clock : in bit; 
           regA,regB: in register_index; 
	   data_in: in  dlx_word; 
	   dataA_out,dataB_out: out dlx_word
	   );
end entity regfile; 

architecture behaviour of regfile is 
type arr is array (31 downto 0) of dlx_word;
signal holder :  arr;
begin
	reg_file : process(clock,read_notwrite,regA,regB,data_in) is
	
	begin 
		if clock = '1' then
			if read_notwrite = '1' then 
			-- Read
				dataA_out <= holder(bv_to_integer(regA));
				dataB_out <= holder(bv_to_integer(regB));
			else 
			-- Write
				holder(bv_to_integer(regA)) <= data_in;
			end if;
		end if;
	end process reg_file;
end architecture behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;

entity DM is
  
  port (
    address : in dlx_word;
    readnotwrite: in bit; 
    data_out : out dlx_word;
    data_in: in dlx_word; 
    clock: in bit); 
end DM;

architecture behaviour of DM is

begin  -- behaviour

  DM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable data_memory : memtype;
  begin
    -- fill this in by hand to put some values in there
    data_memory(1023) := B"00000101010101010101010101010101";
    data_memory(0) := B"00000000000000000000000000000001";
    data_memory(1) := B"00000000000000000000000000000010";
    if clock'event and clock = '1' then
      if readnotwrite = '1' then
        -- do a read
        data_out <= data_memory(bv_to_natural(address)/4);
      else
        -- do a write
        data_memory(bv_to_natural(address)/4) := data_in; 
      end if;
    end if;


  end process DM_behav; 

end behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;

entity IM is
  
  port (
    address : in dlx_word;
    instruction : out dlx_word;
    clock: in bit); 
end IM;

architecture behaviour of IM is

begin  -- behaviour

  IM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable instr_memory : memtype;                   
  begin
    -- fill this in by hand to put some values in there
    -- first instr is 'LW R1,4092(R0)' 
    instr_memory(0) := B"10001100000000010000111111111100";
    -- next instr is 'ADD R2,R1,R1'
    instr_memory(1) := B"00000000001000010001000000100000";
    -- next instr is SW R2,8(R0)'
    instr_memory(2) := B"10101100000000100000000000001000";
    -- next instr is LW R3,8(R0)'
    instr_memory(3) := B"10001100000000110000000000001000"; 
    if clock'event and clock = '1' then
        -- do a read
        instruction <= instr_memory(bv_to_natural(address)/4);
    end if;
  end process IM_behav; 

end behaviour;







