library ieee;
library std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

entity write_data is
	generic(
		file_path		: string := "";
		ENTRIES			: integer := 100;
		WORD_SIZE		: integer := 16;
        ADDR_SIZE		: integer := 16
		);
	port (
		clk				: in std_logic;
		rst             : in std_logic;
		enable			: in std_logic;
		data_in         : in std_logic_vector(WORD_SIZE-1 downto 0)
		);
end write_data;

architecture behavioral of write_data is

	type RWMEM is array (0 to ENTRIES-1) of std_logic_vector(WORD_SIZE-1 downto 0);
	signal memory 	: RWMEM;

	signal addr 	: unsigned(8 downto 0) := (others => '0');

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
	write_proc: process (clk)
	begin
		if rising_edge(clk) then
			if enable = '1' then
				-- WRITE TO MEMORY
				addr <= addr + 1;
				memory(to_integer(unsigned(addr))) <= data_in(WORD_SIZE-1 downto 0);
			end if;
		end if;
	end process;

	refresh_file(memory, file_path); -- refresh the file

end behavioral;