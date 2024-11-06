library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;  
use work.types.all; 

entity exchange is
    generic (
        WORD_BITS :  integer := 32
    );
    port (
        I_SORT    :  in  std_logic;
        I_UP      :  in  std_logic;
        I_A       :  in  ufixed(-1 downto -WORD_BITS);
        I_B       :  in  ufixed(-1 downto -WORD_BITS);
        O_A       :  out ufixed(-1 downto -WORD_BITS);
        O_B       :  out ufixed(-1 downto -WORD_BITS)
    );
end exchange;

architecture rtl of exchange is
begin
    process(I_SORT, I_UP, I_A, I_B)
        variable a_gt_b  :  boolean;
    begin
        a_gt_b := (I_A < I_B);
        if (I_SORT = '1' and I_UP = '1' and a_gt_b = TRUE ) or
           (I_SORT = '1' and I_UP = '0' and a_gt_b = FALSE) then
            O_A <= I_B;
            O_B <= I_A;
        else
            O_A <= I_A;
            O_B <= I_B;
        end if;
    end process;
end rtl;