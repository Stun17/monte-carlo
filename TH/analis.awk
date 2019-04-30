$1 == "high"  { high++   }
$1 == "pair"  { pair++   }
$1 == "dupal" { dupal++  }
$1 == "set"   { set++    }
$1 == "str8"  { str8++   }
$1 == "flush" { flush++  }
$1 == "full"  { full++   }
$1 == "caree" { caree++  }
$1 == "fl-st" { fl-str++ }
END { printf "high   %6.3f\npair   %6.3f\ndupal  %6.3f\nset    %6.3f\nstr8   %6.3f\nflush  %6.3f\nfull   %6.3f\ncaree  %6.3f\nfl-str %6.3f\n", high/NR, pair/NR, dupal/NR, set/NR, str8/NR, flush/NR, full/NR, caree/NR, fl-str/NR } 
