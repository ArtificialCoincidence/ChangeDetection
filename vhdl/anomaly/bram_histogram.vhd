library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use IEEE.math_real.all;

entity bram_histogram is
    generic(
        ADDR_SIZE : integer;
        WORD_SIZE : integer
    );
	port(
		clk 			: in std_logic;
        data_valid      : in std_logic;
		data			: in std_logic_vector(WORD_SIZE-1 downto 0);
		rdaddress		: in std_logic_vector(ADDR_SIZE-1 downto 0);
		wraddress		: in std_logic_vector(ADDR_SIZE-1 downto 0);
		wren			: in std_logic;
		q				: out std_logic_vector(WORD_SIZE-1 downto 0)
	);
end bram_histogram;

architecture behavioral of bram_histogram is

    -- memory
    subtype RAM_ADDR is natural range 0 to (2**ADDR_SIZE-1);
    type RAM_ARRAY is array(RAM_ADDR) of std_logic_vector(WORD_SIZE-1 downto 0);
    signal memory : RAM_ARRAY := (others => (others => '0'));

begin
	
    process(clk) 
	begin
		if rising_edge(clk) then
            if wren = '1' and data_valid = '1' then
                -- WRITE TO MEMORY
                memory(to_integer(unsigned(wraddress))) <= data(WORD_SIZE-1 downto 0); 
            end if;

            -- READ MEMORY
            q <= memory(to_integer(unsigned(rdaddress))); 
        end if;
	end process;

end behavioral;