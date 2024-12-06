library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity histogram is
    generic(
        ADDR_SIZE   : integer := 16;
        WORD_SIZE   : integer := 16
    );
    port (
        clk             : in  std_logic;
        hist_rst        : in  std_logic;
        hist_en         : in  std_logic;
        thrs_en         : in  std_logic;
        ram_addr        : in  std_logic_vector(ADDR_SIZE-1 downto 0); -- read address from threshold or address for reset
        data_in         : in  std_logic_vector(ADDR_SIZE-1 downto 0); -- device data as read address for RAM
        data_out        : out std_logic_vector(ADDR_SIZE-1 downto 0)  -- data to threshold
	);
end histogram;

architecture rtl of histogram is

    component bram_histogram is
        port(
            clock		: in  std_logic;
            data		: in  std_logic_vector(15 downto 0);
            rdaddress	: in  std_logic_vector(15 downto 0);
            wraddress	: in  std_logic_vector(15 downto 0);
            wren		: in  std_logic;
            q		    : out std_logic_vector(15 downto 0)
        );
        end component;
    
    signal ram_data_in      : std_logic_vector(WORD_SIZE-1 downto 0);
    signal ram_data_out     : std_logic_vector(WORD_SIZE-1 downto 0);
    signal ram_wr_add       : std_logic_vector(ADDR_SIZE-1 downto 0);

    signal rd_add_mux       : std_logic_vector(ADDR_SIZE-1 downto 0);
    signal wr_add_mux       : std_logic_vector(ADDR_SIZE-1 downto 0);
    signal ram_data_mux     : std_logic_vector(WORD_SIZE-1 downto 0);
    signal wren, rden       : std_logic;
	 
    signal wr_add_del1      : std_logic_vector(ADDR_SIZE-1 downto 0);

begin

    incr: process(clk, hist_rst)
		variable counter : integer := 1;
    begin
        if hist_rst = '1' then
            wr_add_del1 <= (others => '0');
				ram_data_in <= (others => '0');
        elsif rising_edge(clk) then
            if hist_en = '1' then
					if data_in = wr_add_del1 then
						counter := counter + 1;
					else
						ram_data_in <= ram_data_out + counter;
						counter := 1;
					end if;
            end if;
            wr_add_del1 <= data_in;
				ram_wr_add <= wr_add_del1;
        end if;
    end process;

    BRAM: bram_histogram
	 port map(clk, ram_data_mux, rd_add_mux, wr_add_mux, wren, ram_data_out);

    rd_add_mux <= data_in when thrs_en = '0' else ram_addr;
    wr_add_mux <= ram_wr_add when hist_rst = '0' else ram_addr;
    ram_data_mux <= ram_data_in when hist_rst = '0' else (others => '0');
    wren <= hist_en or hist_rst;
	 
	 data_out <= ram_data_out;
    
end rtl;