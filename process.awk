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
    { # если вызов функции оправдан
	res = $0

	u = index(substr($0, t + 2), "-}")
	while (u == 0)		# пока не найдем конец многострочного комментария
	{	
	    if (getline <= 0)
	    {
		res = "unexpected EOF or error"
		res = (res ": " ERRNO)
		print res > "/dev/stderr"
		exit
	    }
	    res = res $0	# объединяем строки
	    u = index($0, "-}")
	}
	
	sub(/^.*{-[[:space:]]*/, "", res)
	sub(/[[:space:]]*-}.*$/, "", res)
    }

    return res
}
#---------------------------------------------------------------------

BEGIN {
    split("", modules)		# список модулей
    split("", generics)		# список функций
    split("", names)		# список имён функций
    # 
    reverse = 1;
}

/{-[[:space:]]*REGISTER:/ {		# определение для функций вида:
				# {- REGISTER: sum = Module.Name.func :: Integer -> Integer -> Integer -}
				# {- PREGISTER symbol | predicat a1 a3 aN = module.name :: type -> type -> … -> type -}
				# TODO: добавить регистрацию без явного указания типов, но с указанием количества аргументов. Также с предикатом и без
    
    #----------------Извлечение данных-----------------------------
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

    #-------------------Имя----------------------------------------

    names[symbol] = ""
    
    #------------------Модуль--------------------------------------

    modules["import qualified " module "\n"] = "" # добавили модуль

    #-----------------Объявление функции---------------------------
    # Сгенерируем тип функции
    defn = symbol " :: " rep("Dynamic -> ", args) "Maybe Dynamic\n"
    # объявление функции
    defn = defn symbol " " gen_args("arg", " ", args) "\n"

    #------------------Тело функции--------------------------------

    # Эту часть можно хранить в хэш-массиве с ключом defn
    # проверки.
    bodyfn = "    | "
    for (i = 1; i <= args; i++)
	bodyfn = bodyfn "(typeOf (undefined :: " types[i] ") == dynTypeRep arg"i") && "
    bodyfn = bodyfn "((("predicat") >>= (fromDynamic :: (Dynamic -> Maybe Bool))) == (Just True))"

    # тело функции 
    bodyfn = bodyfn " = do\n"
    for (i = 1; i <= args; i++)
	bodyfn = bodyfn "        a"i" <- ((fromDynamic arg"i") :: (Maybe " types[i] "))\n"
    bodyfn = bodyfn "        Just $ toDyn $ ((" name ") :: (" type ")) " gen_args("a", " ", args) "\n"

    #------------Запись в хэш[объявление] = тело-------------------
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
