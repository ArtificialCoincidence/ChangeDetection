library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

entity read_data is
	generic (
		file_path	: string := "";
		ENTRIES		: integer := 100;
		WORD_SIZE	: integer := 32;
        ADDR_SIZE	: integer := 32
        
	);
	port (
		RST					: in std_logic;
		ADDR				: in std_logic_vector(ADDR_SIZE-1 downto 0);
		ENABLE     			: in std_logic;
		DATA_OUT			: out std_logic_vector(WORD_SIZE-1 downto 0)
	);
end read_data;

architecture behavioral of read_data is

	type RWMEM is array (0 to ENTRIES-1) of std_logic_vector(WORD_SIZE-1 downto 0);
	signal memory : RWMEM;

	begin
		mem_proc: process (RST, ENABLE, ADDR)

		file mem_fp: text;
		variable file_line : line;
		variable index : integer;
		variable tmp_data_u : std_logic_vector(WORD_SIZE-1 downto 0);

	begin
		if RST = '0' then
			file_open(
				mem_fp,
				file_path,
				READ_MODE
			);
            
            index := 0;
			while (not endfile(mem_fp)) loop
				readline(mem_fp,file_line);
				hread(file_line,tmp_data_u);
				memory(index) <= std_logic_vector(unsigned(tmp_data_u));
				index := index + 1;
			end loop;

			file_close(mem_fp);

		elsif ENABLE = '1' then
            DATA_OUT <= std_logic_vector(memory(to_integer(unsigned(ADDR))));

		end if;

    end process mem_proc;

end behavioral;