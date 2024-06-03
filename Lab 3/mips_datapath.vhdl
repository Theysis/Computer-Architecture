library lab3;
use lab3.dlx_types.all;
use lab3.bv_arithmetic.all;

entity dlx_register is 
	generic(prop_delay : Time:= 5 ns);
	port(in_val : in dlx_word; clock: in bit; out_val: out dlx_word);
end entity dlx_register;

architecture bit_register of dlx_register is 
begin
	bitregister : process(in_val,clock) is 
	begin
		if clock = '1' then 
			out_val <= in_val after prop_delay;
		end if;
	end process bitregister;	
end architecture bit_register;


library lab3;
use lab3.dlx_types.all;
use lab3.bv_arithmetic.all;

entity mux is 
	generic(prop_delay : Time := 5 ns);
	port (input_1,input_0 : in dlx_word; which: in bit; output: out dlx_word); 
end entity mux;

architecture two_way_multiplexer of mux is 
begin
	two_way_multi : process(which,input_1,input_0) is
	begin 
		if which = '1' then 
			output <= input_1 after prop_delay;
		else 
			output <= input_0 after prop_delay;
		end if;
	end process two_way_multi;
end architecture two_way_multiplexer; 

library lab3;
use lab3.dlx_types.all;
use lab3.bv_arithmetic.all;

entity sign_extend is 
     port (input: in half_word; output: out dlx_word); 
end entity sign_extend; 
architecture sign_extended of sign_extend is 
begin
	sig_extend : process(input) is 
	begin
		output <= (others => input(15));
		output(15 downto 0) <= input;
	end process sig_extend;
end architecture sign_extended;

library lab3;
use lab3.dlx_types.all;
use lab3.bv_arithmetic.all;

entity add4 is 
	generic(prop_delay : Time := 5 ns);
	port (input: in dlx_word; output: out dlx_word); 
end entity add4; 
architecture pc_incrementer of add4 is 
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
end architecture pc_incrementer;

library lab3;
use lab3.dlx_types.all;
use lab3.bv_arithmetic.all;

entity regfile is 
	generic(prop_delay : Time := 5 ns);
	port (read_notwrite,clock : in bit; regA,regB: in register_index; data_in: in dlx_word; dataA_out,dataB_out: out dlx_word); 
end entity regfile; 
architecture registerfile of regfile is 
type arr is array (31 downto 0) of dlx_word;
signal holder :  arr;
begin
	reg_file : process(clock,read_notwrite,regA,regB,data_in) is
	
	begin 
		if clock = '1' then
			if read_notwrite = '1' then 
			-- Read
				dataA_out <= holder(bv_to_integer(regA)) after prop_delay;
				dataB_out <= holder(bv_to_integer(regB)) after prop_delay;
			else 
			-- Write
				holder(bv_to_integer(regA)) <= data_in after prop_delay;
			end if;
		end if;
	end process reg_file;
end architecture registerfile;

