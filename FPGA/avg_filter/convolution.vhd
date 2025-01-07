library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.types.all;

entity convolution is
    generic(
        V_SIZE: integer;
        H_SIZE: integer;
        K_SIZE: integer
    );

    port(
        clk         : in std_logic;
        rst         : in std_logic;
        shift       : in std_logic;
        conv        : in std_logic;
        data_in     : in ufixed(-1 downto -32);
        avg_out     : out ufixed(-1 downto -32)
    );
end convolution;

architecture behavioral of convolution is

    -- number of pixels of partial image
    constant IM_BUF : integer := H_SIZE * (K_SIZE - 1) + K_SIZE;

    -- buffers
    type IMAGE_ARRAY is array(IM_BUF-1 downto 0) of ufixed(-1 downto -32);
    signal matrix_line : IMAGE_ARRAY;

begin
    
    process(clk) is
        variable k_acc : ufixed(6 downto -32);
        variable counter : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                k_acc := (others => '0');
            else
                matrix_line(IM_BUF-1 downto 0) <= matrix_line(IM_BUF-2 downto 0) & data_in;
                
                if shift = '1' then
                    if (counter mod H_SIZE) < K_SIZE then
                        k_acc := resize(k_acc + data_in, 6, -32);
                    end if;

                    if (counter = (K_SIZE - 1) * H_SIZE + K_SIZE) then
                        counter := 0;
                    end if;
                    counter := counter + 1;
                end if;

                if conv = '1' then
                    for i in K_SIZE downto 0 loop
                        k_acc := resize(k_acc - matrix_line(i*H_SIZE + K_SIZE) + matrix_line(i*H_SIZE + K_SIZE-1), 6, -32); -- sub left, add right kernel
                    end loop;
                end if;
            end if;
        end if;

        avg_out <= resize(k_acc/K_SIZE**2, -1, -32);

    end process;
end behavioral;

-- -- Solution, resize the result
-- Y7 <= resize (
--  arg => Y7 + A,
--  size_res => Y7,
--  overflow_style => fixed_saturate,
--  -- fixed_wrap
--  round_style => fixed_round
--  -- fixed_truncate
--  );