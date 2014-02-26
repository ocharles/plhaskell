#include <dlfcn.h>

#include <postgres.h>
#include <executor/spi.h>
#include <commands/trigger.h>
#include <fmgr.h>
#include <access/heapam.h>
#include <utils/syscache.h>
#include "utils/builtins.h"
#include <catalog/pg_proc.h>
#include <catalog/pg_type.h>
#include "postgres.h"
#include "fmgr.h"
#include <string.h>

#include "access/htup_details.h"
#include "access/transam.h"
#include "funcapi.h"
#include "utils/hsearch.h"

#define CAT(a,b) XCAT(a,b)
#define XCAT(a,b) a ## b
#define STR(a) XSTR(a)
#define XSTR(a) #a

#include <HsFFI.h>

#include "PlHaskell_stub.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

//extern void __stginit_PlHaskell(void);
extern void __stginit_plhaskellzm1zi0zi0_PlHaskell(void);

extern void _PG_init(void);

PG_FUNCTION_INFO_V1(plhaskell_call_handler);

void
_PG_init(void)
{
    static char *argv[] = { "plhaskell.so", 0 }, **argv_ = argv;
    static int argc = 1;

    hs_init(&argc, &argv_);
    // hs_add_root(__stginit_PlHaskell);
    hs_add_root(__stginit_plhaskellzm1zi0zi0_PlHaskell);
}

Datum
plhaskell_call_handler(PG_FUNCTION_ARGS)
{
    Oid fn_oid = fcinfo->flinfo->fn_oid;
    HeapTuple procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fn_oid));

    bool isnull;
    Datum prosrcdatum = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_prosrc, &isnull);
    if (isnull)
        elog(ERROR, "null prosrc");

	Form_pg_proc procStruct = (Form_pg_proc) GETSTRUCT(procTup);
    HeapTuple retType = SearchSysCache1(TYPEOID, ObjectIdGetDatum(procStruct->prorettype));
    Form_pg_type rvTypeStruct = (Form_pg_type) GETSTRUCT(retType);

    ReleaseSysCache(procTup);
    ReleaseSysCache(retType);

    uintptr_t *datum = malloc(sizeof(uintptr_t));
    char r = plhaskell_test(TextDatumGetCString(prosrcdatum), rvTypeStruct->typname.data, &datum, fcinfo->arg);
    if (r < 0)
      elog(ERROR, (const char *) (datum));

    if (r == 0)
      return CStringGetTextDatum ((const char*) datum);
    if (r == 1)
      return Int32GetDatum( datum );
}
