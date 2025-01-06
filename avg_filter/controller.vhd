library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.types.all;

entity controller is
    generic(
        V_SIZE: integer;
        H_SIZE: integer;
        K_SIZE: integer
    );

    port(
        clk         : in std_logic;
        rst         : in std_logic;
        start       : in std_logic;

        --eo_image    : out std_logic;
        busy        : out std_logic;
        o_valid     : out std_logic;

        shift       : out std_logic;
        conv        : out std_logic
    );
end controller;

architecture behavioral of controller is

    type filter_state is (IDLE, FILL, COMPUTE);
    signal state        : filter_state := IDLE;

begin
    
    process(clk) is
        variable counter : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state <= IDLE;
                counter := 0;
                busy <= '0';
                shift <= '0';
                conv <= '0';
            else
                case state is
                    when IDLE =>
                        if start = '1' then
                            state <= FILL;
                            shift <= '1';
                        end if;

                    when FILL =>
                        busy <= '1';
                        shift <= '1';
                        if (counter = (K_SIZE - 1) * H_SIZE + K_SIZE) then
                            state <= COMPUTE;
                            shift <= '0';
                        end if;
                        counter := counter + 1;

                    when COMPUTE =>
                        conv <= '1';
                        o_valid <= '1';
                end case;
            end if;
        end if;
    end process;
end behavioral;