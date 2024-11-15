library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity scan_image is
    generic(
        IMAGE_SIZE : integer;
        ADDR_SIZE  : integer;
        WORD_SIZE  : integer
    );
	port(
		clk 			: in std_logic;
		enable			: in std_logic;
		outlayer		: in std_logic;     -- connected to read_value (thresholds)
		data_in         : in std_logic_vector(WORD_SIZE-1 downto 0);    -- from memory (original image)
		data_out		: out std_logic_vector(WORD_SIZE-1 downto 0)    -- back to memory
	);
end scan_image;

architecture behavioral of scan_image is
begin
	
    process(clk) 
	begin
        if rising_edge(clk) then
            if enable = '1' then
                if outlayer = '1' then
                    data_out <= data_in;
                else 
                    data_out <= (others=>'0');
                end if;
            end if;
        end if;
	end process;

end behavioral;