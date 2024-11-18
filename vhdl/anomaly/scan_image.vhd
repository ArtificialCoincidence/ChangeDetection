library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity scan_image is
    generic(
        IM_SIZE     : integer;
        ADDR_SIZE   : integer;
        WORD_SIZE   : integer
    );
	port(
		clk 	    : in  std_logic;
        rst         : in  std_logic;
		enable		: in  std_logic;     -- data valid signal negated
        start       : in  std_logic;
        thrs1       : in  std_logic_vector(ADDR_SIZE-1 downto 0);
		data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);   -- from memory (original image)
		data_out	: out std_logic_vector(WORD_SIZE-1 downto 0);   -- back to memory
        addr_out    : out std_logic_vector(ADDR_SIZE-1 downto 0);   -- write address
        e_o_i       : out std_logic
	);
end scan_image;

architecture behavioral of scan_image is

    -- memory
    subtype BUFFER_ADDR is natural range 0 to (IM_SIZE-1);
    type BUFFER_ARRAY is array(BUFFER_ADDR) of std_logic_vector(WORD_SIZE-1 downto 0);

    signal buffer_im    : BUFFER_ARRAY := (others => (others => '0'));

begin
	
    process(clk)
        variable cntr_store, cntr_scan : integer := 0;
	begin
        if rst = '1' then
            cntr_store := 0;
            cntr_scan := 0;
            e_o_i <= '0';
            addr_out <= (others => '0');
        elsif rising_edge(clk) then
            if enable = '0' then
                if cntr_store <= IM_SIZE-1 then
                    buffer_im(cntr_store) <= data_in; 
                    cntr_store := cntr_store + 1;
                end if;
            end if;
            
            if start = '1' then
                if cntr_scan < IM_SIZE-1 then
                    if data_in >= thrs1 then
                        data_out <= buffer_im(cntr_scan);
                    else
                        data_out <= (others => '0');
                    end if;
                    cntr_scan := cntr_scan + 1;
                else 
                    e_o_i <= '1';
                end if;
            end if;
        end if;

        addr_out <= std_logic_vector(to_unsigned(cntr_scan, addr_out'length));
	end process;

end behavioral;