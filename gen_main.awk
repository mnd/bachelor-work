#!/usr/bin/gawk -f 

/module/,/where/ {
    S = S " " $0
    if (/where/) {
	if ((S ~ /symbolTable/) && (S ~ /readTable/) && (S ~ /showTable/)) {
	    match(S, /module[[:space:]]+(['.[:alnum:]]+)[[:space:]]*\(/, arr)
	    name = arr[1];
	    modules[name] = 1;
	}
	S = ""
    }
}

END {
    print "import DynamicFace"
    for (m in modules) {
	if (!m || (m == "DynamicFace")) continue;
	print "import qualified", m
    }

    printf("%s\n", "main = repl $ defaultPD {");
    printf("%s", "  importModules = [");
    for (m in modules) {
	if (!m || (m == "DynamicFace")) continue;
	if (F) printf("%s", ", ");
	F = 1;
	printf("(\"%s\", (%s.symbolTable, %s.readTable, %s.showTable))", m, m, m, m);
    }
    printf("%s\n", "]");    
    printf("%s\n", "  }");    
}