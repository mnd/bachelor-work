#!/usr/bin/gawk -f 

function ltrim(s)
{
    sub(/^[[:space:]]+/, "", s);
    return s
}
function rtrim(s)
{
    sub(/[[:space:]]+$/, "", s);
    return s
}
function trim(s)
{
    return rtrim(ltrim(s));
}
function join(array, start, end, sep,    result, i)
{
    if (sep == "")
	sep = " "
    else if (sep == SUBSEP) # magic value
	sep = ""
    result = array[start]
    for (i = start + 1; i <= end; i++)
	result = result sep array[i]
    return result
}

function rep(string,num,      res)
{
    while (num-- > 0) res = res string;
    return res;
}
function gen_args(string, delim, num,      res, i)
{
    for (i = 1; i < num; i++) res = res string i delim;
    return res string i;
}
function get_whole_comment(res, u, t)
{
    if ((t = index($0, "{-")) != 0)
    { # if $0 contain {- 
	res = $0

	u = index(substr($0, t + 2), "-}")
	while (u == 0)		# while end of comment not found
	{	
	    if (getline <= 0)
	    {
		res = "unexpected EOF or error"
		res = (res ": " ERRNO)
		print res > "/dev/stderr"
		exit
	    }
	    res = res $0	# concatenating string
	    u = index($0, "-}")
	}
	
	sub(/^.*{-[[:space:]]*/, "", res)
	sub(/[[:space:]]*-}.*$/, "", res)
    }

    return res
}
#---------------------------------------------------------------------

BEGIN {
    split("", modules)		# modules list
    split("", generics)		# functions list
    split("", names)		# function names list
    # 
    reverse = 1;
}

/{-[[:space:]]*REGISTER:/ {	# Function definition:
				# {- REGISTER: sum = Module.Name.func :: Integer -> Integer -> Integer -}
				# {- REGISTER symbol | predicat a1 a3 aN = module.name :: type -> type -> … -> type -}
    
    #----------------String Parsing-----------------------------
    str = get_whole_comment()
    where = match(str, /^REGISTER:[[:space:]]*([[:alnum:]]+)[[:space:]]*\|([^=]*)=[[:space:]]*(([[:alnum:].]+)\.[[:alnum:]]+)[[:space:]]*::[[:space:]]*(.*)$/, arr)
    if (where)
    {
	symbol = arr[1]
	predicat = trim(arr[2])
	name = arr[3]
	module = arr[4]
	type = arr[5]
    }
    else
    {
	where = match(str, /^REGISTER:[[:space:]]*([[:alnum:]]+)[[:space:]]*=[[:space:]]*(([[:alnum:].]+)\.[[:alnum:]]+)[[:space:]]*::[[:space:]]*(.*)$/, arr)
	if (where)
	{ 
	    symbol = arr[1]
	    predicat = "Just $ toDyn $ True"
	    name = arr[2]
	    module = arr[3]
	    type = arr[4]
	}
	else next;
    }

    split(type, types, /[[:space:]]*->[[:space:]]*/)
    args = length(types) - 1
    # for (i=1; i <= args+1; i++)
    # 	types[i] = module"."types[i]
    # type = join(types, 1, args+1, " -> ")

    #-------------------Name----------------------------------------

    names[symbol] = ""
    
    #------------------Module--------------------------------------

    modules["import qualified " module "\n"] = "" # append module to module list

    #-----------------Function type---------------------------
    # Сгенерируем тип функции
    defn = symbol " :: " rep("Dynamic -> ", args) "Maybe Dynamic\n"
    # объявление функции
    defn = defn symbol " " gen_args("arg", " ", args) "\n"

    #------------------function body--------------------------------

    # tests
    bodyfn = "    | "
    for (i = 1; i <= args; i++)
	bodyfn = bodyfn "(typeOf (undefined :: " types[i] ") == dynTypeRep arg"i") && "
    bodyfn = bodyfn "((("predicat") >>= (fromDynamic :: (Dynamic -> Maybe Bool))) == (Just True))"

    # body
    bodyfn = bodyfn " = do\n"
    for (i = 1; i <= args; i++)
	bodyfn = bodyfn "        a"i" <- ((fromDynamic arg"i") :: (Maybe " types[i] "))\n"
    bodyfn = bodyfn "        Just $ toDyn $ ((" name ") :: (" type ")) " gen_args("a", " ", args) "\n"

    #------------Write hash[type] = body-------------------
    if (reverse)
	generics[defn] = bodyfn generics[defn]
    else
	generics[defn] = generics[defn] bodyfn
}

END {
    print "module Dynamic"
    print "       ("
    for (n in names)       printf "         %s,\n", n
    print "       ) where"
    print ""
    for (m in modules)     printf "%s", m
    print "import Data.Typeable"
    print "import Data.Dynamic"
    print "import Data.Maybe"
    print ""
    for (inst in generics) print inst generics[inst]
}
