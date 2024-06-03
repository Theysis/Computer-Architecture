entity oneBitAdder is
	generic(prop_delay: Time := 10 ns);
	port(carry_in,a_in,b_in: in bit;
		result,carry_out: out bit);
end entity oneBitAdder;


architecture behaviour1 of oneBitAdder is
begin
	adderProcess : process(carry_in,a_in,b_in) is
	begin
		if carry_in = '0' then
			if a_in = '1' then
			  if b_in = '1' then
				-- 0,1,1 => 0,1
				result <= '0' after prop_delay;
				carry_out <= '1' after prop_delay;
			  else
				-- 0,1,0 => 1,0
				result <= '1' after prop_delay;
			  	carry_out <= '0' after prop_delay;
			  end if;
			else
			  if b_in = '1' then
				-- 0,0,1 => 1,0
				result <= '1' after prop_delay;
				carry_out <= '0' after prop_delay;
			  else 
				-- 0,0,0 => 0,0
				result <= '0' after prop_delay;
				carry_out <= '0' after prop_delay;
			  end if;
			end if;
		else
			if a_in = '1' then
			  if b_in = '1' then
				-- 1,1,1 => 1,1
				result <= '1' after prop_delay;
				carry_out <= '1' after prop_delay;
			  else
				-- 1,1,0 => 0,1
				result <= '0' after prop_delay;
			  	carry_out <= '1' after prop_delay;
			  end if;
			else
			  if b_in = '1' then
				-- 1,0,1 => 0,1
				result <= '0' after prop_delay;
				carry_out <= '1' after prop_delay;
			  else 
				-- 1,0,0 => 1,0
				result <= '1' after prop_delay;
				carry_out <= '0' after prop_delay;
			  end if;
			end if;
		end if;		
	end process adderProcess;
end architecture behaviour1;