library ieee;
library std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.fixed_pkg.all;
--library floatfixlib;
--use floatfixlib.fixed_pkg.all;

entity read_data is
	generic (
		file_path	: string 	:= "";
		ENTRIES		: integer 	:= 100;
		WORD_SIZE	: integer 	:= 16;
		ADDR_SIZE	: integer 	:= 16
        
	);
	port (
		clk			: in std_logic;
		rst			: in std_logic;
		enable     	: in std_logic;
		data_out		: out std_logic_vector(WORD_SIZE-1 downto 0);
		valid			: out std_logic;
		endpacket	: out std_logic;
		startpacket	: out std_logic
	);
end read_data;

architecture behavioral of read_data is

	type RWMEM is array (0 to ENTRIES-1) of std_logic_vector(WORD_SIZE-1 downto 0);
	signal memory 	: RWMEM;

	signal addr 	: unsigned(8 downto 0) := (others => '0');

	begin
		mem_proc: process (clk)

		file mem_fp			: text;
		variable file_line 	: line;
		variable index 		: integer;
		variable tmp_data_u : real;

	begin
		if rising_edge(clk) then
			if rst = '1' then
				startpacket <= '0';
				endpacket <= '0';
				valid <= '1';
				
				file_open(
					mem_fp,
					file_path,
					READ_MODE
				);

				index := 0;
				while ((not endfile(mem_fp)) and (index <= ENTRIES-1)) loop
					readline(mem_fp,file_line);
					read(file_line,tmp_data_u);
					memory(index) <= std_logic_vector(to_ufixed(tmp_data_u, 0, -(WORD_SIZE-1)));
					index := index + 1;
				end loop;

				file_close(mem_fp);

			elsif enable = '1' then
				if addr <= 499 then
					startpacket <= '0';
					endpacket <= '0';
					valid <= '1';
					data_out <= memory(to_integer(unsigned(addr)));
					addr <= addr + 1;
					
					if addr = 0 then
						startpacket <= '1';
					elsif addr = 499 then
						endpacket <= '1';
					end if;
				else
					valid <= '0';
					startpacket <= '0';
					endpacket <= '0';			
				end if;
			end if;
		end if;
    end process mem_proc;
end behavioral;