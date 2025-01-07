library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity scan_image is
	generic(
		ADDR_SIZE   : integer := 16;
		WORD_SIZE   : integer := 16
	);
	port(
		clk 	    	: in  std_logic;
		scan_rst   	: in  std_logic;
		scan_en		: in  std_logic;
		thrs_val    : in  std_logic_vector(ADDR_SIZE-1 downto 0);
		data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);
		data_out		: out std_logic_vector(WORD_SIZE-1 downto 0)
	);
end scan_image;

architecture rtl of scan_image is
begin

	process(clk, scan_rst)
	begin
        if scan_rst = '1' then
            data_out <= (others => '0');
        elsif rising_edge(clk) then            
            if scan_en = '1' then
					if data_in >= thrs_val then
						data_out <= data_in;
					else
						data_out <= (others => '0');
					end if;
            end if;
        end if;
	end process;

end rtl;