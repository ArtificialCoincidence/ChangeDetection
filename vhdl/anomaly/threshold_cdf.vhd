library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity threshold_cdf is
    generic (
        IMAGE_SIZE: integer;
        WORD_SIZE : integer;    -- Width of each histogram bin (number of bits)
        ADDR_SIZE : integer     -- Log of number of histogram bins
    );
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        enable      : in  std_logic;    -- histogram complete, start executing
        read_value  : out std_logic;    -- high between 3rd and 4th quantile, flag
        datain      : in  std_logic_vector(WORD_SIZE-1 downto 0);  -- Histogram value from BRAM
        rdaddr      : out std_logic_vector(ADDR_SIZE-1 downto 0)  -- Output threshold value (address)
    );
end threshold_cdf;

architecture behavioral of threshold_cdf is

    constant tot_pixels : integer := IMAGE_SIZE*IMAGE_SIZE;

    signal address_ramp : unsigned(ADDR_SIZE-1 downto 0) := (others => '0');
    signal cumulative_sum : unsigned(WORD_SIZE-1 downto 0) := (others => '0');

begin

    process(clk, rst)
        variable pixel_count : integer := 0;
    begin
        if rst = '1' then
            address_ramp   <= (others => '0');
        elsif rising_edge(clk) then
            if enable = '1' then
                rdaddr <= std_logic_vector(pixel_count);
                if pixel_count >= tot_pixels then
                    pixel_count := 0;
                end if;
            end if;
        end if;
    end process;

    process(datain)
        variable cumulative : unsigned(BIN_WIDTH-1 downto 0);
        constant 3quantile : integer := tot_pixels*3/4;
        constant 4quantile : integer := tot_pixels;
    begin
        if rst = '1' then
            cumulative_sum <= (others => '0');
            read_value     <= '0';
        else
            cumulative := cumulative + unsigned(datain);
            if cumulative >= 3quantile and cumulative <= 4quantile then
                read_value <= '1';
            end if;
        end if;

        cumulative_sum <= std_logic_vector(cumulative);
    end process;

end behavioral;