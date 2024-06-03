-- lab4_control_v3.vhd

use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity chops up a 32-bit word into the relevant component parts.
-- If a particular output is not used for a particular instruction type
-- that field is set to zero. The input from the decoder is the instruction
-- register. It operates in a purely combinational-logic mode. The controller
-- makes use of its outputs when appropriate, ignores them otherwise.
-- For R-type ALU instruction format in Figure 2.27, 
-- reg0p1 is labelled "rs" in Figure 2.27, regOp2 is labelled "rt", and
-- regDest is labelled "rd".
-- For I-type ALU instruction format in Figure 2.27
-- regOp1 is "rs" and regDest is "rt"

entity mips_decoder is
  generic(prop_delay: Time := 5 ns);
  port (
    instruction : in dlx_word;
    regOp1,regOp2,regDest: out register_index;
    alu_func: out func_code; 
    immediate: out half_word;
    opcode: out opcode_type   
  ); 
end mips_decoder;

architecture behaviour of mips_decoder is 

begin
	
	decodebehave : process(instruction) is 
	variable rs : register_index; 
	variable rt : register_index;
	variable rd : register_index;
	variable opOut : opcode_type;
	begin
		rs := instruction(25 downto 21);
		rt := instruction(20 downto 16);
		rd := instruction(15 downto 11);
		opOut := instruction(31 downto 26);
		opcode <= opOut;
		
		-- R-type instruction
		if opOut = "000000" then
			regOp1 <= rs after prop_delay;
			regOp2 <= rt after prop_delay;
			regDest <= rd after prop_delay;
			immediate <= instruction(15 downto 0) after prop_delay;
			alu_func <= instruction(5 downto 0);
		-- I-type instruction		
		elsif opOut = "100011" then 
			regOp1 <= rs after prop_delay;
			regOp2 <= rt after prop_delay;
			regDest <= (others => '0') after prop_delay;
			alu_func <= (others => '0') after prop_delay;
			immediate <= instruction(15 downto 0) after prop_delay;
		else
			regOp1 <= rs after prop_delay;
			regOp2 <= (others => '0') after prop_delay;
			regDest <= rt after prop_delay;
			alu_func <= (others => '0') after prop_delay;
			immediate <= instruction(15 downto 0) after prop_delay;
		end if;
	end process decodebehave;
end architecture behaviour;


use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity controls the DLX processor. It is driven by the external
-- clock signal, and takes inputs from the decoder also. It drives the
-- input of every latch on the chip, and the control input to every
-- mux, as well as sending function codes
-- to the ALU and processing ALU error codes

entity mips_controller is
  generic(prop_delay: Time := 5 ns);
  port (
    opcode: in  opcode_type;
    alu_func: in func_code;
    clock: in bit; 
    aluA_mux: out bit;
    aluB_mux: out bit;
    alu_oper: out alu_operation_code;
    alu_signed: out bit; 
    write_mux: out bit;
    ir_clock: out bit;
    IM_clock: out bit; 
    pc_clock: out bit;
    npc_clock: out bit;
    imm_clock: out bit;
    alu_out_clock: out bit; 
    lmd_clock: out bit; 
    regA_clock,regB_clock: out bit;
    DM_clock: out bit;
    DM_readnotwrite: out bit;
    reg_clock: out bit;
    reg_readnotwrite: out bit;
    regA_index_mux: out bit; 
    zero_out: in bit; -- only applies to jump implentation
    cond_out: out bit -- only applies to jump implentation
    );
    
end mips_controller;

architecture behaviour of mips_controller is

begin  -- behaviour

  behav: process(opcode,clock) is
     -- cuurent state of the machine 
     type state_type is range 1 to 5;                                
     variable state: state_type := 1;
                               
  begin                                
     if clock'event and clock = '1' then
       case state is 
         when 1 =>
		
	        -- Clocks
    		ir_clock <= '1' after prop_delay; 
    		IM_clock <= '1' after prop_delay; 
    		pc_clock <= '0' after prop_delay; 
    		npc_clock <= '1' after prop_delay; 
    		imm_clock <= '0' after prop_delay; 
   		alu_out_clock <= '0' after prop_delay; 
   		lmd_clock <= '0' after prop_delay; 
  		regA_clock <= '0' after prop_delay; 
		regB_clock <= '0' after prop_delay; 
		DM_clock <= '0' after prop_delay; 
  		reg_clock <= '0' after prop_delay;
		
		--Multiplexers 
		aluA_mux <= '0' after prop_delay; 
   		aluB_mux <= '0' after prop_delay; 
    		alu_oper <= "0000" after prop_delay; -- alu_operation_code;
    		alu_signed <= '0' after prop_delay;  
    		write_mux <= '0' after prop_delay; 
		regA_index_mux <= '0' after prop_delay;  

		-- Read/Write
		DM_readnotwrite <= '0' after prop_delay; 
		reg_readnotwrite <= '0' after prop_delay; 
		
		-- Other 
		cond_out <= '0' after prop_delay; -- only applies to jump implentation

            	state := 2;      
         when 2 =>
	        -- Clocks
    		ir_clock <= '0' after prop_delay; 
    		IM_clock <= '0' after prop_delay; 
    		pc_clock <= '0' after prop_delay; 
    		npc_clock <= '0' after prop_delay; 
    		imm_clock <= '1' after prop_delay; 
   		alu_out_clock <= '0' after prop_delay; 
   		lmd_clock <= '0' after prop_delay; 
  		regA_clock <= '1' after prop_delay; 
		regB_clock <= '1' after prop_delay; 
		DM_clock <= '0' after prop_delay; 
  		reg_clock <= '1' after prop_delay;
		
		--Multiplexers 
		aluA_mux <= '0' after prop_delay; 
   		aluB_mux <= '0' after prop_delay; 
    		alu_oper <= "0000" after prop_delay; -- alu_operation_code;
    		alu_signed <= '0' after prop_delay;  

		regA_index_mux <= '1' after prop_delay;  

		-- Read/Write
		DM_readnotwrite <= '0' after prop_delay; 
		reg_readnotwrite <= '0' after prop_delay; 
		write_mux <= '0' after prop_delay; 
		
		-- Other 
		cond_out <= '0' after prop_delay; -- only applies to jump implentation
            	
		state := 3; 
         when 3 =>
		
		 -- Clocks
    		ir_clock <= '0' after prop_delay; 
    		IM_clock <= '0' after prop_delay; 
    		pc_clock <= '0' after prop_delay; 
    		npc_clock <= '0' after prop_delay; 
    		imm_clock <= '0' after prop_delay; 
   		alu_out_clock <= '1' after prop_delay; 
   		lmd_clock <= '0' after prop_delay; 
  		regA_clock <= '1' after prop_delay; 
		regB_clock <= '1' after prop_delay; 
		DM_clock <= '0' after prop_delay; 
  		reg_clock <= '1' after prop_delay;
		
		
		
		-- Register-Register Operations
		if alu_func = "100000" and opcode = "000000" then -- ADD
			alu_signed <= '1' after prop_delay; 
			alu_oper <= x"0" after prop_delay; -- alu_operation_code;
		elsif alu_func = "100001" then 
			if opcode = "000000" then -- ADDU
				alu_signed <= '0' after prop_delay; 
				alu_oper <= x"0" after prop_delay; -- alu_operation_code;
			elsif opcode = "001000" then -- ADDI
				alu_signed <= '1' after prop_delay; 
				alu_oper <= x"0" after prop_delay; -- alu_operation_code;
			elsif opcode = "001001" then -- ADDUI
				alu_signed <= '0' after prop_delay; 
				alu_oper <= x"0" after prop_delay; -- alu_operation_code;
			end if;
		elsif alu_func = "100010" then -- SUB
			if opcode = "001000" or opcode = "001010" then
				alu_signed <= '1' after prop_delay; 
				alu_oper <= x"1" after prop_delay; -- alu_operation_code;
			end if;
		elsif alu_func = "100011" and opcode = "000000" then -- SUBU
			alu_signed <= '0' after prop_delay; 
			alu_oper <= x"1" after prop_delay; -- alu_operation_code;
		elsif alu_func = "001110" and opcode = "000000" then -- MUL
			alu_signed <= '1' after prop_delay; 
			alu_oper <= x"e" after prop_delay; -- alu_operation_code;
		elsif alu_func = "010110" and opcode = "000000" then -- MULU
			alu_signed <= '0' after prop_delay; 
			alu_oper <= x"e" after prop_delay; -- alu_operation_code;
		elsif alu_func = "100100" and (opcode = "001100" or opcode = "000000")  then -- AND 
			alu_signed <= '0' after prop_delay; 
			alu_oper <= x"2" after prop_delay; -- alu_operation_code;
		elsif alu_func = "100101" and opcode = "000000" then
			alu_signed <= '0' after prop_delay; 
			alu_oper <= x"3" after prop_delay; -- alu_operation_code;
		elsif alu_func = "101010" and opcode = "000000" then
			alu_signed <= '1' after prop_delay; 
			alu_oper <= x"b" after prop_delay; -- alu_operation_code;
		end if;
		
			

		-- Register-immediate
		--Multiplexers 
		aluA_mux <= '1' after prop_delay; 
   		aluB_mux <= '1' after prop_delay; 

 

		regA_index_mux <= '0' after prop_delay;  

		-- Read/Write
		DM_readnotwrite <= '0' after prop_delay; 
		reg_readnotwrite <= '0' after prop_delay; 
		write_mux <= '0' after prop_delay;


            state := 4; 
         when 4 =>
		-- Clocks
    		ir_clock <= '0' after prop_delay; 
    		IM_clock <= '0' after prop_delay; 
    		pc_clock <= '0' after prop_delay; 
    		npc_clock <= '0' after prop_delay; 
    		imm_clock <= '0' after prop_delay; 
   		alu_out_clock <= '1' after prop_delay; 
   		lmd_clock <= '0' after prop_delay; 
  		regA_clock <= '0' after prop_delay; 
		regB_clock <= '1' after prop_delay; 
		DM_clock <= '1' after prop_delay; 
  		reg_clock <= '0' after prop_delay;
		
		--Multiplexers 
		aluA_mux <= '0' after prop_delay; 
   		aluB_mux <= '0' after prop_delay; 
    		alu_oper <= "0000" after prop_delay; -- alu_operation_code;
    		alu_signed <= '0' after prop_delay;  

		regA_index_mux <= '0' after prop_delay;  

		-- Read/Write
		DM_readnotwrite <= '1' after prop_delay; 
		reg_readnotwrite <= '0' after prop_delay; 
		write_mux <= '0' after prop_delay; 
		
		-- Other 
		cond_out <= '0' after prop_delay; -- only applies to jump implentation
            	


            state := 5; 
         when 5 =>
		
		-- Clocks
    		ir_clock <= '0' after prop_delay; 
    		IM_clock <= '0' after prop_delay; 
    		pc_clock <= '1' after prop_delay; 
    		npc_clock <= '0' after prop_delay; 
    		imm_clock <= '0' after prop_delay; 
   		alu_out_clock <= '1' after prop_delay; 
   		lmd_clock <= '1' after prop_delay; 
  		regA_clock <= '0' after prop_delay; 
		regB_clock <= '0' after prop_delay; 
		DM_clock <= '0' after prop_delay; 
  		reg_clock <= '0' after prop_delay;
		
		--Multiplexers 
		aluA_mux <= '0' after prop_delay; 
   		aluB_mux <= '0' after prop_delay; 
    		alu_oper <= "0000" after prop_delay; -- alu_operation_code;
    		alu_signed <= '0' after prop_delay;  

		regA_index_mux <= '0' after prop_delay;  

		-- Read/Write
		DM_readnotwrite <= '0' after prop_delay; 
		reg_readnotwrite <= '0' after prop_delay; 
		write_mux <= '1' after prop_delay; 
		
		-- Other 
		cond_out <= '0' after prop_delay; -- only applies to jump implentation

            state := 1; 
         when others => null;
       end case;
     else
       if clock'event and clock = '0' then 
		--set register clocks back to zero
		ir_clock <= '0' after prop_delay;
    		IM_clock <= '0' after prop_delay; 
    		pc_clock <= '0' after prop_delay;
    		npc_clock <= '0' after prop_delay;
    		imm_clock <= '0' after prop_delay;
    		alu_out_clock <= '0' after prop_delay;
    		lmd_clock <= '0' after prop_delay; 
    		regA_clock <= '0' after prop_delay;
		regB_clock <= '0' after prop_delay;
    		DM_clock <= '0' after prop_delay;
    		reg_clock <= '0' after prop_delay;
       end if;
       
     end if;
  end process behav;                                 

end behaviour;





