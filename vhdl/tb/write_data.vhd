library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
library std;
use std.textio.all;

entity write_data is
	generic(
		file_path		: string := "";
		ENTRIES			: integer := 100;
		WORD_SIZE		: integer := 16;
        ADDR_SIZE		: integer := 16
		);
	port (
		CLK				: in std_logic;
		RST             : in std_logic;
		ADDR		    : in std_logic_vector(ADDR_SIZE-1 downto 0);
		ENABLE			: in std_logic;
		DATA_IN         : in std_logic_vector(WORD_SIZE-1 downto 0)
		);
end write_data;

architecture behavioral of write_data is

	type RWMEM is array (0 to ENTRIES-1) of std_logic_vector(WORD_SIZE-1 downto 0);
	signal memory : RWMEM;

	procedure refresh_file(data_mem: in RWMEM; path_file: string) is
		variable index		: natural range 0 to ENTRIES;
		file wr_file		: text;
		variable line_in	: line;
	begin
		index:=0;
		file_open(wr_file, path_file, WRITE_MODE);
		while index <= ENTRIES-1 loop
			write(line_in, data_mem(index));
			writeline(wr_file, line_in);
			index := index + 1;
		end loop;
	end refresh_file;

begin  
	write_proc: process (CLK)
	begin
		if rising_edge(CLK) then
			if ENABLE = '1' then
				-- WRITE TO MEMORY
				memory(to_integer(unsigned(ADDR))) <= DATA_IN(WORD_SIZE-1 downto 0);
			end if;
		end if;
	end process;

	refresh_file(memory, file_path); -- refresh the file

end behavioral;