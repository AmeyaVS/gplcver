/* Copyright (c) 1991-2004 Pragmatic C Software Corp. */

/*
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.
 
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
 
   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place, Suite 330, Boston, MA, 02111-1307.
 
   There is also a commerically supported faster new version of Cver that is
   not released under the GPL.   See file commerical-cver.txt, or web site
   www.pragmatic-c.com/commercial-cver or contact sales@pragmatic-c.com to
   learn more about commerical Cver.
   
 */


/*
 * third source module reads tasks/functions, udps and specify section
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef __DBMALLOC__
#include "../malloc.h"
#endif

/* REMOVEME - no longer supporting SunOS - maybe needed for hpux? */
#if defined(__sparc) && !defined(__SVR4)  
extern int tolower(int);
#endif

#include "v.h"
#include "cvmacros.h"

/* local prototypes */
static struct udp_t *alloc_udp(struct sy_t *);
static int rd_udp_hdr(struct udp_t *);
static int rd_udp_decls(void);
static int rd_udp_init(struct udp_t *);
static int chkcnt_uports(struct udp_t *);
static int rd_udp_table(struct udp_t *);
static void str_tolower(char *, char *);
static int cvrt_udpedges(char *, char *);
static int to_udp_levsym(char);
static int chk_comb_udpline(char *, struct udp_t *, int *);
static int chk_sequdpline(char *, struct udp_t *, int *);
static char to_edgech(int);
static int is_edgesym(char);
static char *to_codedge_line(char *, char *);
static void extra_chk_edgeudp(struct udp_t *);
static char *to_udp_prtnam(struct udp_t *, int);
static void dmp_udp_lines(FILE *, struct udp_t *);
static struct spfy_t *alloc_spfy(void);
static int rd_specparamdecl(void);
static void assign_1specparam(struct net_t *, struct expr_t *, int, int);
static int rd_delay_pth(void);
static struct exprlst_t *rd_pthtermlst(void);
static int col_pthexpr(void);
static int rd_pathdelaylist(struct paramlst_t **);
static void init_spcpth(struct spcpth_t *);
static int rd_setup_or_hold_tchk(unsigned);
static int rd_tchk_part(unsigned, struct tchk_t *, struct expr_t **);
static int rd_setuphold_tchk(void);
static int rd_recrem_tchk(void);
static int rd_width_tchk(void);
static int rd_period_tchk(void);
static int rd_skew_recov_rem_tchk(unsigned);
static int rd_nochg_tchk(void);
static int rd_tchk_selector(int *, struct expr_t **, struct expr_t **);
static int rd_edges(int *);
static struct sy_t *rd_notifier(void);
static struct attr_t *chk_dup_attrs(struct attr_t *);

/* extern prototypes (maybe defined in this module) */
extern char *__pv_stralloc(char *);
extern char *__my_malloc(int);
extern char *__my_realloc(char *, int , int);
extern void __my_free(char *, int);
extern char *__prt_vtok(void);
extern char *__prt_kywrd_vtok(void);
extern char *__to_uvvnam(char *, word);
extern char *__to_tcnam(char *, unsigned);
extern char *__to_sytyp(char *, unsigned);
extern struct sy_t *__get_sym(char *, struct symtab_t *);
extern struct sy_t *__decl_sym(char *, struct symtab_t *);
extern struct sy_t *__bld_loc_symbol(int, struct symtab_t *, char *, char *);
extern struct exprlst_t *__alloc_xprlst(void);
extern struct tnode_t *__vtfind(char *, struct symtab_t *);
extern struct symtab_t *__alloc_symtab(int);
extern struct expr_t *__alloc_newxnd(void);
extern struct mod_pin_t *__alloc_modpin(void);
extern struct paramlst_t *__alloc_pval(void);
extern struct expr_t *__bld_rng_numxpr(word, word, int);
extern int __vskipto_modend(int);
extern void __add_sym(char *, struct tnode_t *);
extern int __chk_redef_err(char *, struct sy_t *, char *, unsigned);
extern void __remove_undef_mod(struct sy_t *);
extern void __get_vtok(void);
extern void __unget_vtok(void);
extern void __dmp_udp(FILE *, struct udp_t *);
extern int __udp_vskipto_any(int);
extern int __udp_vskipto2_any(int, int);
extern int __udp_vskipto3_any(int, int, int);
extern int __wide_vval_is0(register word *, int);
extern void __wrap_puts(char *, FILE *);
extern void __wrap_putc(int, FILE *);
extern void __nl_wrap_puts(char *, FILE *);
extern int __fr_tcnam(char *);
extern int __spec_vskipto_any(int);
extern int __spec_vskipto2_any(int, int);
extern int __spec_vskipto3_any(int, int, int);
extern int __rd_opt_param_vec_rng(struct expr_t **, struct expr_t **);
extern int __col_paramrhsexpr(void);
extern int __col_connexpr(int);
extern int __col_comsemi(int);
extern void __bld_xtree(int);
extern int __src_rd_chk_paramexpr(struct expr_t *, int);
extern void __set_numval(struct expr_t *, word, word, int);
extern struct net_t *__add_param(char *, struct expr_t *, struct expr_t *);
extern int __col_parenexpr(int);
extern int __bld_expnode(void);
extern void __set_xtab_errval(void);
extern int __col_delexpr(void);
extern int __vskipto3_modend(int, int, int);
extern void __init_tchk(struct tchk_t *, unsigned);
extern void __set_0tab_errval(void);
extern void __free_xtree(struct expr_t *);
extern void __free2_xtree(struct expr_t *);
extern void __skipover_line(void);
extern int __my_getlin(register char *);
extern int __pop_vifstk(void);
extern int __open_sfil(void);
extern void __do_include(void);
extern void __do_foreign_lang(void);
extern void __exec_vpi_langlinecbs(char *, char *, int);
extern int __notokenize_skiplines(char *);
extern char *__bld_lineloc(char *, unsigned, int);
extern char *__match_cdir(char *, char *);
extern int __exec_rdinserted_src(char *);
extern void __push_vinfil(void);
extern void __rd_ver_mod(void);
extern int __expr_has_glb(struct expr_t *);
extern struct xstk_t *__eval2_xpr(struct expr_t *);
extern void __cnv_stk_fromreg_toreal(struct xstk_t *, int);
extern void __sizchgxs(struct xstk_t *, int);
extern char *__msgexpr_tostr(char *, struct expr_t *);
extern void __cnv_stk_fromreal_toreg32(struct xstk_t *);
extern void __eval_param_rhs_tonum(struct expr_t *);
extern int __cmp_xpr(struct expr_t *, struct expr_t *);

extern void __cv_msg(char *s, ...);
extern void __pv_ferr(int, char *, ...);
extern void __pv_fwarn(int, char *, ...);
extern void __gfinform(int, unsigned, int, char *, ...);
extern void __gfwarn(int, unsigned, int, char *, ...);
extern void __gferr(int, unsigned, int, char *, ...);
extern void __ia_err(int, char *, ...);
extern void __dbg_msg(char *, ...);
extern void __pv_terr(int, char *, ...);
extern void __arg_terr(char *, int);
extern void __case_terr(char *, int);
extern void __misc_terr(char *, int);
extern void __misc_fterr(char *, int);

extern word __masktab[];

/*
 * UDP PROCESSING ROUTINES
 */

/*
 * process a udp definition
 * added to end of list of udps with header __udphead
 * name of udp has already been read
 * notice there is a separate design wide list of udps
 *
 * primitive keyword read and reads endprimitive
 */
extern int __rd_udpdef(void)
{
 int retval, initlcnt, initsfnind;
 struct udp_t *udpp;
 struct tnode_t *tnp;
 struct sy_t *syp;

 initlcnt = 0;
 initsfnind = 0;
 /* notice that Verilog keywords are reserved words */
 retval = TRUE;
 /* for now must be able to declare to continue syntax checking */
 if (__toktyp != ID)
  {
   __pv_ferr(1155, "udp name expected - %s read", __prt_kywrd_vtok());
skip_end:
   __letendnum_state = TRUE;
   retval = __vskipto_modend(ENDPRIMITIVE);
   __letendnum_state = FALSE;
   return(retval);
  }
 tnp = __vtfind(__token, __modsyms);
 if (__sym_is_new)
  {
   __add_sym(__token, tnp);
   (__modsyms->numsyms)++;
   syp = tnp->ndp;
  }
 else
  {
   syp = tnp->ndp;
   /* if previously guessed as module, will just change */
   if (!__chk_redef_err(__token, syp, "udp", SYM_UDP)) goto skip_end;
   /* chk fail means never in module undef list */
  __remove_undef_mod(syp);
  }
 syp->sytyp = SYM_UDP;
 udpp = alloc_udp(syp);
 syp->el.eudpp = udpp;
 syp->sydecl = TRUE;
 /* need place where udp declared */
 syp->syfnam_ind = __cur_fnam_ind;
 syp->sylin_cnt = __lin_cnt;

 /* must also allocate the new symbol table */
 /* udps have no internal structure, sym table discarded when done */
 udpp->usymtab = __alloc_symtab(FALSE);
 __cur_udp = udpp;
 /* link symbol table back to module symbol */
 udpp->usymtab->sypofsyt = syp;

 /* do not need to build type until entire module read */
 /* any return here means skipped to endprimitive or next file level */
 if (!rd_udp_hdr(udpp)) return(FALSE);
 if (!rd_udp_decls()) return(FALSE);
 if (__toktyp == INITial)
  {
   initlcnt = __lin_cnt;
   initsfnind = __cur_fnam_ind;
   if (!rd_udp_init(udpp)) return(FALSE);
   __get_vtok();
   if (__toktyp != TABLE)
    {
     __pv_ferr(1156, "udp table section missing - %s read", __prt_vtok());
     goto skip_end;
    }
  }
 /* sets type to U_LEVEL if not combinatorial - edge type detected later */
 if (!chkcnt_uports(udpp)) retval = FALSE;
 if ((int) udpp->numstates >= __ualtrepipnum) udpp->u_wide = TRUE;
 else udpp->u_wide = FALSE;

 /* this reads the endprimitive */
 if (!rd_udp_table(udpp)) return(FALSE);
 if (udpp->utyp == U_COMB && udpp->ival != NO_VAL)
  {
   __gferr(1157, initsfnind, initlcnt,
    "combinatorial udp %s cannot have initial value", syp->synam);
   udpp->ival = NO_VAL;
  }

 __get_vtok();
 if (__toktyp != ENDPRIMITIVE)
  {
   __pv_ferr(1158,
    "udp endprimitive keyword expected - %s read", __prt_vtok());
   goto skip_end;
  }
 if (!retval) return(TRUE);

 /* catch common extra ; error here */
 __get_vtok();
 if (__toktyp == SEMI)
  __pv_ferr(1152, "semicolon following endprimitive illegal"); 
 else __unget_vtok();

 extra_chk_edgeudp(udpp);

 /* notice if error before here not added to list */ 
 if (__udp_last == NULL) __udphead = udpp; else __udp_last->udpnxt = udpp;
 __udp_last = udpp;
 if (__debug_flg) __dmp_udp(stdout, udpp);
 return(TRUE);
}

/*
 * allocate a udp
 */
static struct udp_t *alloc_udp(struct sy_t *syp)
{
 struct udp_t *udpp;

 udpp = (struct udp_t *) __my_malloc(sizeof(struct udp_t));
 udpp->usym = syp;
 udpp->usymtab = NULL;
 udpp->upins = NULL;
 udpp->utyp = U_COMB;
 udpp->numins = 0;
 udpp->numstates = 0;
 udpp->u_used = FALSE;
 udpp->u_wide = FALSE;
 /* initial value - assume none that becomes 1'bx for level */
 udpp->ival = NO_VAL;
 udpp->utlines = NULL;
 udpp->udpnxt = NULL;
 udpp->utab = NULL;
 udpp->uidnum = 0;
 return(udpp);
}

/*
 * read the udp header
 * only simple variable names allowed in this header
 * reads ending ;
 * handles skipping - 
 */
static int rd_udp_hdr(struct udp_t *udpp)
{
 struct mod_pin_t *upp, *last_upp;
 struct sy_t *syp;

 /* empty I/O list illegal */
 __get_vtok();
 if (__toktyp == SEMI)
  {
   __pv_ferr(1162, "required udp header list of ports missing");
   return(TRUE);
  }

 if (__toktyp != LPAR)
  {
   __pv_ferr(1164,
    "udp header list of ports initial left parenthesis expected - %s read",
    __prt_vtok());
   if (__udp_vskipto2_any(RPAR, SEMI)) 
    {
     if (__toktyp == RPAR) __get_vtok();
     /* if not semi, assume semi left out - if bad, next rout. will catch */
     if (__toktyp != SEMI) __unget_vtok();
     return(TRUE);
    }
ret_end:
   if (__syncto_class == SYNC_FLEVEL) return(FALSE);
   else return(TRUE);
  }
 __get_vtok();
 if (__toktyp == RPAR)
  {
   __pv_ferr(1165, "empty udp header list of ports () form illegal");
do_end:
   __get_vtok();
   if (__toktyp == SEMI) return(TRUE);
   __pv_ferr(980,
    "module header list of ports end semicolon expected - %s read",
    __prt_vtok());
   __unget_vtok();
   return(TRUE);
  }
 for (last_upp = NULL;;)
  {
   /* this declares the symbols in the header */
   if (__toktyp != ID)
    {
     __pv_ferr(1166, "udp port variable name expected - %s read",
      __prt_kywrd_vtok());
do_resync:
     if (__udp_vskipto3_any(COMMA, SEMI, RPAR)) 
      {
       /* port effectively not seen - error emitted already */
       if (__toktyp == COMMA) goto nxt_port;
       if (__toktyp == RPAR) goto do_end;
       return(TRUE);
      }
     goto ret_end;
    }
   syp = __decl_sym(__token, __cur_udp->usymtab);
   /* must fill in connection to port still */
   if (__sym_is_new) syp->sytyp = SYM_N;
   else
    {
     __pv_ferr(1167,
      "udp port %s repeated in header list of ports", syp->synam);
     goto nxt_port;
    }
   upp = __alloc_modpin();
   upp->mptyp = IO_UNKN;
   upp->mpsnam = syp->synam;
   upp->mpref = NULL;
   syp->el.empp = upp;

   if (last_upp == NULL) udpp->upins = upp; else last_upp->mpnxt = upp;
   last_upp = upp;

nxt_port:
   __get_vtok();
   if (__toktyp == RPAR)
    {
     __get_vtok();
     if (__toktyp == SEMI) break;
     __pv_ferr(1168,
      "udp header list of ports ending semicolon expected - %s read",
      __prt_vtok());
     goto do_end;
    }
   if (__toktyp != COMMA)
    {
     __pv_ferr(1169,
      "udp header comma or semicolon separator expected - %s read",
      __prt_vtok());
     goto do_resync;
    }
   __get_vtok();
  }
 return(TRUE);
}

/*
 * read the udp declarations
 * must read first port type and reads following initial or table
 * eliminates illegal vector ports by not parsing
 */
static int rd_udp_decls(void)
{
 struct mod_pin_t *mpp;
 struct sy_t *syp;
 int outdecl_seen, regdecl_seen;

 regdecl_seen = outdecl_seen = FALSE;
 for (;;)
  {
again:
   __get_vtok();
   if (__toktyp == INITial || __toktyp == TABLE) break;
   switch ((byte) __toktyp) {
    case INPUT:
     for (;;)
      {
       __get_vtok();
       if (__toktyp != ID)
        {
         __pv_ferr(1170, "udp input port name expected - %s read",
	  __prt_kywrd_vtok());
sync_in:
         if (__udp_vskipto2_any(COMMA, SEMI)) 
          {
           /* port effectively not seen - error emitted already */
           if (__toktyp == COMMA) continue;
           goto again;
          }
         if (__syncto_class == SYNC_FLEVEL) return(FALSE);
         else goto again;
	}
       if ((syp = __get_sym(__token, __cur_udp->usymtab)) == NULL)
        {
not_in_port:
          __pv_ferr(1173,
           "udp input declaration of \"%s\" - non header input port", __token);
         goto nxt_port;
        }
       if (syp->sytyp != SYM_N)
	{
bad_sytab:
	 /* udp symbol table inconsistent */
         __misc_fterr(__FILE__, __LINE__);
	}
       mpp = syp->el.empp;
       if (syp->sydecl || mpp->mptyp != IO_UNKN) goto not_in_port;
       mpp->mptyp = IO_IN;
       syp->sydecl = TRUE;

nxt_port:
       __get_vtok();
       if (__toktyp == SEMI) break;
       if (__toktyp == COMMA) continue;
       __pv_ferr(1174,
        "udp port declaration comma or semicolon separator expected - %s read",
        __prt_vtok());
       goto sync_in;
      }
     break;
    case OUTPUT:
     if (outdecl_seen)
      {
       __pv_ferr(1178, "only one udp output port declaration permitted");
       __get_vtok();
       goto end_out_port;
      }
     outdecl_seen = TRUE;

      __get_vtok();
     if (__toktyp != ID)
      {
       __pv_ferr(1179, "udp output port name expected - %s read",
        __prt_kywrd_vtok());
sync_out:
       if (__udp_vskipto_any(SEMI)) return(TRUE); 
       if (__syncto_class == SYNC_FLEVEL) return(FALSE);
       else goto again;
      }
     if ((syp = __get_sym(__token, __cur_udp->usymtab)) == NULL)
      {
not_out_port:
       __pv_ferr(1180,
        "udp output port declaration of \"%s\" that is not in header port list",
        __token);
       goto end_out_port;
      }
     if (syp->sytyp != SYM_N) goto bad_sytab;
     mpp = syp->el.empp;
     /* NON_IO means already declared as reg so nothing to do */
     if (mpp->mptyp != NON_IO)
      {
       if (syp->sydecl || mpp->mptyp != IO_UNKN) goto not_out_port;
       mpp->mptyp = IO_OUT;
       syp->sydecl = TRUE;
      }
end_out_port:
     __get_vtok();
     if (__toktyp != SEMI)
      {
       __pv_ferr(1181,
        "udp output declaration not followed by semicolon - %s read",
        __prt_vtok());
       goto sync_out;
      }
     break;
    case REG:
     if (regdecl_seen)
      {
       __pv_ferr(1182, "only one udp reg declaration permitted");
       __get_vtok();
       goto end_reg;
      }
     regdecl_seen = TRUE;
     __get_vtok();
     if (__toktyp != ID)
      {
       __pv_ferr(1183,
        "udp reg declaration output port name expected - %s read",
        __prt_kywrd_vtok());
sync_reg:
       if (__udp_vskipto_any(SEMI)) return(TRUE); 
       if (__syncto_class == SYNC_FLEVEL) return(FALSE);
       else goto again;
      }
     if ((syp = __get_sym(__token, __cur_udp->usymtab)) == NULL)
      {
not_reg_port:
       __pv_ferr(1184,
        "udp reg declaration of \"%s\" that is not in header port list",
        __token);
       goto end_reg;
      }
     if (syp->sytyp != SYM_N) goto bad_sytab;
     mpp = syp->el.empp;
     if (mpp->mptyp == IO_OUT) mpp->mptyp = NON_IO;
     else
      {
       /* if not output must be undeclared */
       if (syp->sydecl || mpp->mptyp != IO_UNKN) goto not_reg_port;
       mpp->mptyp = NON_IO;
      }
end_reg:
     __get_vtok();
     if (__toktyp != SEMI)
      {
       __pv_ferr(1187,
        "udp output reg declaration ending semicolon expected - %s read",
        __prt_vtok());
       goto sync_reg;
      }
     break;
    default:
     __pv_ferr(1188,
      "udp declaration I/O declaration keyword expected - %s read",
      __prt_vtok());
    return(FALSE);
   }
  }
 return(TRUE);
}

/*
 * read the one optional initial statement for the one udp output
 * know initial read - format is intial [output term] = [1 bit const]
 * complicated because no mechanism for conversion from 32 1 bit vals
 */
static int rd_udp_init(struct udp_t *udpp)
{
 int blen;

 __get_vtok();
 if (__toktyp != ID) goto bad_init;
 __get_vtok();
 if (__toktyp != EQ) goto bad_init;
 __get_vtok();
 if (__toktyp != NUMBER) goto bad_init;
 __get_vtok();
 if (__toktyp != SEMI) goto bad_init;
 if (__itoklen > WBITS)
  {
   blen = __itoklen - WBITS;
   if (!vval_is0_(&(__acwrk[1]), blen)) goto bad_val; 
   if (!vval_is0_(&(__bcwrk[1]), blen)) goto bad_val; 
  }
 /* this must be a 1 bit value but wider with all zero's ok */
 if (__acwrk[0] == 0L && __bcwrk[0] == 0L) udpp->ival = 0;
 else if (__acwrk[0] == 1L && __bcwrk[0] == 0L)
  udpp->ival = 1;
 else if (__acwrk[0] == 0L && __bcwrk[0] == 1L)
  {
   __pv_fwarn(576, "udp initial value 1'bz illegal - changed to 1'bx");
   udpp->ival = 3;
  }
 else if (__acwrk[0] == 1L && __bcwrk[0] == 1L) udpp->ival = 3;
 else
  {
bad_val:
   __pv_ferr(1191, "udp initial value must be one of: 0, 1, 1'bx - %s read",
    __prt_vtok());
   udpp->ival = NO_VAL;
  }
 return(TRUE);

bad_init:
 __pv_ferr(1192, "udp initial statement syntax error");
 if (__udp_vskipto_any(SEMI)) return(TRUE);
 if (__syncto_class == SYNC_FLEVEL) return(FALSE);
 return(TRUE);
}

/*
 * check and count number of ports and set to default sequential if needed
 * number of ports in number of inputs (+ 1 if level or edge)
 * udp type number udp inputs and type set here
 * error if header port not declared
 * return FALSE on error
 */
static int chkcnt_uports(struct udp_t *udpp)
{
 register struct mod_pin_t *mpp;
 int retval;
 int unumins, unumstates;

 mpp = udpp->upins;
 retval = TRUE;
 if (mpp->mptyp == IO_IN)
  {
   __gferr(1193, udpp->usym->syfnam_ind, udpp->usym->sylin_cnt,
    "first udp header port %s not the output", mpp->mpsnam);
   retval = FALSE;
  }
 unumins = unumstates = 0;
 if (mpp->mptyp == NON_IO) { udpp->utyp = U_LEVEL; unumstates++; }

 mpp = mpp->mpnxt;
 for (; mpp != NULL; mpp = mpp->mpnxt)
  {
   if (mpp->mptyp != IO_IN)
    {
     __gferr(1194, udpp->usym->syfnam_ind, udpp->usym->sylin_cnt,
      "after first udp port %s must be an input", mpp->mpsnam);
     retval = FALSE;
    }
   unumins++;
  }
 udpp->numins = unumins;
 udpp->numstates = unumins + unumstates;
 if (udpp->numins > MAXUPRTS)
  {
   __gferr(1195, udpp->usym->syfnam_ind, udpp->usym->sylin_cnt,
    "udp definition has %d ports - maximum allowed is %d",
    udpp->numins, MAXUPRTS);
   retval = FALSE;
  }
 return(retval);
}

/*
 * read the udp table
 * know table keyword read and reads endtable
 * when done udp lines are array of chars of length numins + 1 (for out)
 * numins includes state for non combinatorial
 *
 * if edge, 1 allowed edge, char in line is 2nd plus uledinum index of edge
 * and ultabsel is 1st (maybe wildcard) - need to convert back to r/f abbrev.
 */
static int rd_udp_table(struct udp_t *udpp)
{
 int ulcnt, has_wcard, ulfnam_ind;
 struct utline_t *utlp, *last_utlp;
 char uline[RECLEN], coduline[RECLEN], s1[RECLEN];

 __letendnum_state = TRUE;
 /* initialized for malformed ; only line - error caught later */
 ulfnam_ind = __cur_fnam_ind;
 ulcnt = __lin_cnt;

 for (last_utlp = NULL;;)
  {
   __get_vtok();
   if (__toktyp == ENDTABLE) break;
   strcpy(uline, "");
   for (;;)
    {
     if (__toktyp == SEMI) goto end_line;

     /* need beginning of udp entry line no. */
     ulfnam_ind = __cur_fnam_ind;
     ulcnt = __lin_cnt;
     switch ((byte) __toktyp) {
      case LPAR: strcat(uline, "("); break;
      case RPAR: strcat(uline, ")"); break;
      case QUEST: strcat(uline, "?"); break;
      case MINUS: strcat(uline, "-"); break;
      case TIMES: strcat(uline, "*"); break;
      case COLON: strcat(uline, ":"); break;
      /* this requires that even non sized numbers stored in token */
      /* works because ' not legal in udp table line */
      case ID: strcat(uline, __token); break;
      /* SJM 03/20/00 - must assemble from num token for numbers */
      case NUMBER: strcat(uline, __numtoken); break;
      default:
       __pv_ferr(1198, "invalid udp table line symbol %s", __prt_vtok());
       if (__udp_vskipto_any(SEMI)) continue;
       if (__toktyp == ENDTABLE) goto done;
       if (__syncto_class == SYNC_FLEVEL) goto bad_end;
       __vskipto_modend(ENDPRIMITIVE);
bad_end:
       __letendnum_state = FALSE;
       return(FALSE);
     }
     __get_vtok();
    }
end_line:
   /* at this point line collected and contains only 1 char values and punct */
   /* utyp only U_COMB if does not have reg declaration */
   if (udpp->utyp == U_COMB)
    {
     __cur_utabsel = NO_VAL;
     __cur_ueipnum = NO_VAL;
     str_tolower(coduline, uline);
     if (!chk_comb_udpline(coduline, udpp, &has_wcard)) goto bad_end;
    }
   else
    {
     str_tolower(s1, uline);
     /* edge temporarily converted to 0x80 form */
     if (!cvrt_udpedges(coduline, s1)) return(FALSE);
     /* edge converted to 1st char __cur_utabsel plus 2nd char here in uline */
     __cur_utabsel = NO_VAL;
     if (!chk_sequdpline(coduline, udpp, &has_wcard)) continue;
    }
   utlp = (struct utline_t *) __my_malloc(sizeof(struct utline_t));
   utlp->tline = __pv_stralloc(coduline);
   utlp->ullen = (unsigned) strlen(coduline);
   utlp->ulhas_wcard = (has_wcard) ? TRUE : FALSE;
   utlp->uledinum = __cur_ueipnum;
   utlp->utabsel = __cur_utabsel;
   utlp->utlfnam_ind = ulfnam_ind;
   utlp->utlin_cnt = ulcnt;
   utlp->utlnxt = NULL;
   if (last_utlp == NULL) udpp->utlines = utlp; else last_utlp->utlnxt = utlp;
   last_utlp = utlp;
  }
done:
 __letendnum_state = FALSE;
 return(TRUE);
}

static void str_tolower(char *to, char *from)
{
 while (*from)
  {
   if (isupper(*from)) *to++ = (char) tolower(*from); else *to++ = *from;
   from++;
  }
 *to = '\0';
}

/*
 * convert (..) form edges to one coded char - real edge processing
 * in check seqential udp line routine
 * edge has high bit on and then bits 5-3 is e1 and 2-0 is e2
 *
 * know all legal upper case edges converted to lower case before called
 * first step in udp table line checking - edge abbreviation not seen here
 */
static int cvrt_udpedges(char *culine, char *uline)
{
 register char *culp, *ulp;
 char *chp, ech1, ech2;
 int ie1, ie2, no_err;
 char s1[RECLEN];

 no_err = FALSE;
 for (no_err = TRUE, ulp = uline, culp = culine;;)
  {
   switch (*ulp) {
    case '\0': *culp = '\0'; goto done;
    case '(':
     ech1 = *(++ulp);
     if ((ie1 = to_udp_levsym(ech1)) == -1)
      {
       __pv_ferr(1202,
        "udp table line edge first symbol '%c' not a level symbol", ech1);
edge_err:
       no_err = FALSE;
       /* making error into (? ?) edge */
       *culp++ = (char) (0x80 + (UV_Q << 3) + UV_Q);
       if ((chp = strchr(ulp, ')')) == NULL) { *culp = '\0'; return(no_err); }
       ulp = ++chp;
       continue;
      }
     ech2 = *(++ulp);
     if ((ie2 = to_udp_levsym(ech2)) == -1)
      {
       __pv_ferr(1203,
        "udp table line edge second symbol '%c' not a level symbol", ech2);
       goto edge_err;
      }
     if (*(++ulp) != ')')
      {
       __pv_ferr(1204,
        "udp table line edge symbol closing ')' expected - %c read", *ulp);
       goto edge_err;
      }
     /* edge has high bit on and bits 5-3 is e1 and 2-0 is e2 */
     *culp++ = (char) (0x80 | (ie1 << 3) | ie2);
     ulp++;
     break;
    default:
     *culp++ = *ulp++;
    }
  }
done:
 if (__debug_flg)
  __dbg_msg("&&& converted from %s to %s\n", uline, to_codedge_line(s1,
   culine));
 return(no_err);
}

/*
 * return value of level symbol (-1 if not level symbol)
 */
static int to_udp_levsym(char ch)
{
 switch (ch) {
  case '0': return(UV_0);
  case '1': return(UV_1);
  case 'x': return(UV_X);
  case '?': return(UV_Q);
  case 'b': return(UV_B);
 }
 return(-1);
}

/*
 * check coded combinatorial udp uline
 */
static int chk_comb_udpline(char *uline, struct udp_t *udpp,
 int *has_wcard)
{
 register char *chp;
 int ins;
 char ch;

 *has_wcard = FALSE;
 /* separate off ending :[output] */
 if ((chp = strrchr(uline, ':')) == NULL)
  {
   __pv_ferr(1205,
    "combinatorial udp line expected ':' output separator missing");
   return(FALSE);
  }
 *chp++ = '\0';
 ch = *chp++;
 if (ch == '-')
  {
   __pv_ferr(1206,
    "combinatorial udp line '-' output symbol illegal - has no state");
   return(FALSE);
  }
 if (ch != '0' && ch != '1' && ch != 'x')
  {
   __pv_ferr(1207,
    "combinatorial udp line output symbol (%x) '%c' illegal", ch, ch);
   return(FALSE);
  }
 if (*chp != '\0')
  {
   __pv_ferr(1208,
    "combinatorial udp line has second output symbol '%c' - only one allowed",
    *chp);
   return(FALSE);
  }
 /* check inputs - up to rightmost : */
 for (chp = uline, ins = 0; *chp != '\0'; chp++, ins++)
  {
   switch (*chp) {
    case '(': case 'r': case 'f': case 'p': case 'n': case '*':
     __pv_ferr(1209, "edge symbol %c illegal in combinatorial udp line", *chp); 
     return(FALSE);
   }
   if (to_udp_levsym(*chp) == -1)
    {
     __pv_ferr(1213,
      "combinatorial udp line input symbol '%c' (position %d) not a level symbol",
      *chp, ins + 1);
     return(FALSE);
    }
   if (ins >= 254)
    {
     __pv_ferr(1214,
      "udp has so many inputs (%d) - it cannot be checked", 254); 
     return(FALSE);
    }
   if (*chp == 'b' || *chp == '?') *has_wcard = TRUE;
  }
 if (ins != udpp->numins)
  {
   __pv_ferr(1215,
    "combinatorial udp line wrong number of input symbols (%d) - should be %d",
    ins, udpp->numins);
   return(FALSE);
  }
 /* finally add output as last symbol */
 *chp++ = ch;
 *chp = '\0';
 return(TRUE);
}

/*
 * check coded sequential upd uline
 * know all (..) edge convert to 1 char by here (0x80 on)
 * if no edge __cur_ueipnum has value NO_VAL
 * old 0x80 bit on form edge converted to 1st as __cur_utabsel, 2nd as char 
 * unless edge wildcard (i.e. r,f,n, etc.) in which case has edge wildcard
 */
static int chk_sequdpline(char *uline, struct udp_t *udpp,
 int *has_wcard)
{
 register char *chp;
 char ch1, ch2;
 int ins, t1;

 *has_wcard = FALSE;
 /* separate off : before previous state :[prev. state]:[output] */
 if ((chp = strchr(uline, ':')) == NULL)
  {
   __pv_ferr(1216,
    "sequential udp line expected colon before old state symbol missing");
   return(FALSE);
  }
 /* end uline */
 *chp = '\0';
 chp++;
 /* ch1 is state symbol and -1 means not 1 of legasl 0,1,x,?,b */
 ch1 = *chp++;
 if (to_udp_levsym(ch1) == -1)
  {
   __pv_ferr(1218,
    "sequential udp line state level symbol '%c' illegal", ch1);
   return(FALSE);
  }
 /* state can be wildcard but not edge */
 if (ch1 == 'b' || ch1 == '?') *has_wcard = TRUE;
 if (*chp++ != ':')
  {
   __pv_ferr(1219,
    "sequential udp line expected colon before output symbol missing");
   return(FALSE);
  }
 /* ch2 is output symbol */
 ch2 = *chp++;
 if (ch2 != '0' && ch2 != '1' && ch2 != 'x' && ch2 != '-')
  {
   __pv_ferr(1221, "sequential udp line output symbol '%c' illegal", ch2);
   return(FALSE);
  }
 if (*chp != '\0')
  {
   __pv_ferr(1222,
    "sequential udp line has extra output symbol '%c' - only one allowed",
    *chp);
   return(FALSE);
  }
 /* previous state and output done, finally check inputs */
 __cur_utabsel = NO_VAL;
 __cur_ueipnum = NO_VAL;
 for (chp = uline, ins = 0; *chp != '\0'; chp++, ins++)
  {
   /* know if edge, already checked - wild card only for level sensitive */
   if (is_edgesym(*chp))
    {
     if (__cur_ueipnum != NO_VAL)
      {
       __pv_ferr(1223,
       "sequential udp line has more than one edge symbol (second input %d)",
        ins + 1);
       return(FALSE);
      }
     __cur_ueipnum = ins;
     if ((*chp & 0x80) != 0)
      {
       /* know if (..) edge then both letters are edge symbols */
       /* edge has high bit on and then bits 5-3 is ie1 and 2-0 is ie2 */
       t1 = *chp & 0x7f;
       *chp = to_edgech(t1 & 0x7);
       __cur_utabsel = to_edgech((t1 >> 3) & 0x7);
       if ((__cur_utabsel == '0' && *chp == '0')
	|| (__cur_utabsel == '1' && *chp == '1')
	|| (__cur_utabsel == 'x' && *chp == 'x'))
	{
 	 __pv_ferr(1224,
	 "sequential udp line edge (%c%c) (input %d) illegal - no transition",
	  __cur_utabsel, *chp, __cur_ueipnum + 1);
         return(FALSE);
        }
      }
     else if (*chp == 'r') { __cur_utabsel = '0'; *chp = '1'; }
     else if (*chp == 'f') { __cur_utabsel = '1'; *chp = '0'; }
     /* if special edge abbrev. but not r or f make both edges have abbrev. */
     else __cur_utabsel = *chp;
     continue;
    }
   if (to_udp_levsym(*chp) == -1)
    {
     __pv_ferr(1225,
      "sequential udp line symbol '%c' (input %d) not edge or level", *chp,
      ins + 1);
     return(FALSE);
    }
   if (*chp == 'b' || *chp == '?') *has_wcard = TRUE;
  }
 if (ins != udpp->numstates - 1 || ins != udpp->numins)
  {
   __pv_ferr(1226,
    "sequential udp line wrong number of input symbols (%d) - should be %d",
    ins, udpp->numins - 1);
   return(FALSE);
  }
 /* finally add previous state input and next state output as last 2 */
 *chp++ = ch1;
 *chp++ = ch2;
 *chp = '\0';
 if (__cur_ueipnum != NO_VAL) udpp->utyp = U_EDGE;
 return(TRUE);
}

/*
 * convert edge 4 bit encoding to normal level edge symbol
 */
static char to_edgech(int encodee)
{
 switch ((byte) encodee) {
  case UV_0: return('0');
  case UV_1: return('1');
  case UV_X: break;
  case UV_Q: return('?');
  case UV_B: return('b');
  default: __case_terr(__FILE__, __LINE__);
 }
 return('x');
}

/*
 * return T if symbol is an edge symbol (code 0x80 or edge letter)
 */
static int is_edgesym(char ch)
{
 if ((ch & 0x80) != 0) return(TRUE);
 switch (ch) {
  case 'r': case 'f': case 'p': case 'n': case '*': return(TRUE);
 }
 return(FALSE);
}

/*
 * convert a coded edge line to a string 
 * in form during input before converted to line separate edge char and 
 * edge destination in line char position 
 */
static char *to_codedge_line(char *s, char *culine)
{
 register char *cpi, *cpo;
 int uch;

 for (cpi = culine, cpo = s; *cpi != '\0'; cpi++)
  {
   if ((*cpi & 0x80) != 0)
    {
     *cpo++ = '(';
     /* notice 5-3 are e1 and 2-0 are e2 */
     uch = (int) *cpi;
     *cpo++ = to_edgech((uch >> 3) & 0x7);
     *cpo++ = to_edgech(uch & 0x7);
     *cpo++ = ')'; 
    }
   else *cpo++ = *cpi;
  }
 *cpo = '\0';
 return(s);
}

/*
 * perform some consistency checks on edge udps
 * - input column probably needs edge in some row
 * - output column probably needs - somewhere
 */
static void extra_chk_edgeudp(struct udp_t *udpp)
{
 register int i;
 int out_hasbar;
 struct utline_t *utlp;

 for (i = 0; i < (int) udpp->numins; i++)
  {
   for (utlp = udpp->utlines; utlp != NULL; utlp = utlp->utlnxt)
    {
     if (utlp->uledinum == i) goto next;
    }
   __gfinform(421, udpp->usym->syfnam_ind, udpp->usym->sylin_cnt,
    "sequential udp \"%s\" column for input %s lacks any edge(s)",
     udpp->usym->synam, to_udp_prtnam(udpp, i + 1));
next:;
  }
 /* inputs are 0 to num ins - 1 then previous state, then next state */
 i = udpp->numins + 1;
 out_hasbar = FALSE;
 for (utlp = udpp->utlines; utlp != NULL; utlp = utlp->utlnxt)
  {
   if (utlp->tline[i] == '-') out_hasbar = TRUE;
  }
 if (!out_hasbar)
  {
   __gfinform(422, udpp->usym->syfnam_ind, udpp->usym->sylin_cnt,
    "sequential udp %s output column lacks any no change (-) symbols",
    udpp->usym->synam);
  }
}

/*
 * find input position %d (first is output)
 * know position number legal and starts at 1
 */
static char *to_udp_prtnam(struct udp_t *udpp, int inum)
{
 register struct mod_pin_t *mpp;
 int nins;

 nins = 1;
 mpp = udpp->upins->mpnxt;
 for (; mpp != NULL; mpp = mpp->mpnxt, nins++)
  {
   if (nins == inum) goto done;
  }
 __misc_terr(__FILE__, __LINE__);
done:
 return(mpp->mpsnam);
}

/*
 * UDP DUMP ROUTINES - FOR DEBUGGING
 */

/*
 * dump a udp for debugging
 * f cannot be nil to put in string
 */
extern void __dmp_udp(FILE *f, struct udp_t *udpp)
{
 register struct mod_pin_t *mpp;
 int first_time;
 char s1[RECLEN], s2[RECLEN];

 if (f == NULL) __arg_terr(__FILE__, __LINE__);
 __cv_msg("\n");
 __cur_sofs = 0;
 __outlinpos = 0;
 __pv_stlevel = 0;

 __wrap_puts("primitive ", f);
 __wrap_puts(udpp->usym->synam, f);
 first_time = TRUE;
 /* notice udp module pin lists continue to use next pointer */
 for (mpp = udpp->upins; mpp != NULL; mpp = mpp->mpnxt)
  {
   if (first_time) { __wrap_putc('(', f); first_time = FALSE; }
   else __wrap_puts(", ", f);
   /* know udp pins must be named */
   __wrap_puts(mpp->mpsnam, f);
  }
 __nl_wrap_puts(");", f);

 /* notice here mpsnam must exist or earlier syntax error */
 __pv_stlevel = 1;
 mpp = udpp->upins;
 __wrap_puts("output ", f);
 __wrap_puts(mpp->mpsnam, f);
 __wrap_putc(';', f);
 if (udpp->utyp != U_COMB)
  {
   __wrap_puts("  reg ", f);
   __wrap_puts(mpp->mpsnam, f);
   __wrap_putc(';', f); 
  }
 __nl_wrap_puts("", f);
 
 __wrap_puts("input ", f);
 first_time = TRUE;
 for (mpp = mpp->mpnxt; mpp != NULL; mpp = mpp->mpnxt)
  {
   if (first_time) first_time = FALSE; else __wrap_puts(", ", f);
   __wrap_puts(mpp->mpsnam, f);
  }
 __nl_wrap_puts(";", f);

 if (udpp->ival != NO_VAL)
  {
   __wrap_puts("initial ", f);
   __wrap_puts(udpp->upins->mpsnam, f);
   sprintf(s1, " = 1'b%s", __to_uvvnam(s2, (word) udpp->ival));
   __wrap_puts(s1, f);
   __nl_wrap_puts(";", f);
  }
 __nl_wrap_puts("", f);
 __nl_wrap_puts("table", f);
 dmp_udp_lines(f, udpp);
 __nl_wrap_puts("endtable", f);
 __pv_stlevel--;
 __nl_wrap_puts("endprimitive", f);
}

/*
 * dump udp lines
 * need to also dump initial value
 */
static void dmp_udp_lines(FILE *f, struct udp_t *udpp)
{
 register int i;
 register struct utline_t *utlp;
 int numins, sav_stlevel;
 char *chp, s1[RECLEN];

 sav_stlevel = __pv_stlevel;
 __pv_stlevel = 4;
 __outlinpos = 0;
 numins = udpp->numins;
 for (utlp = udpp->utlines; utlp != NULL; utlp = utlp->utlnxt)
  {
   for (chp = utlp->tline, i = 0; i < numins; i++, chp++)
    {    
     /* the one possible edge */
     if (utlp->uledinum == i)
      {
       /* special wild card types edges are kept as original char */
       /* 01 and 10 are converted from rise-fall - convert back */
       if (utlp->utabsel == '0' && *chp == '1') __wrap_puts("    r", f);
       else if (utlp->utabsel == '1' && *chp == '0') __wrap_puts("    f", f);
       else if (utlp->utabsel == '*') __wrap_puts("    *", f);
       else if (utlp->utabsel == 'p') __wrap_puts("    p", f);
       else if (utlp->utabsel == 'n') __wrap_puts("    n", f);
       else
        {
         sprintf(s1, " (%c%c)", (char) utlp->utabsel, *chp);
         __wrap_puts(s1, f);
        }
      }
     /* various wild cards and states left as input char */
     else { sprintf(s1, "%5c", *chp); __wrap_puts(s1, f); }
    }
   if (udpp->utyp != U_COMB)
    { sprintf(s1, " : %c ", *chp); __wrap_puts(s1, f); chp++; }
   sprintf(s1, " : %c ;", *chp);
   __nl_wrap_puts(s1, f);
  }
 __pv_stlevel = sav_stlevel;
}

/*
 * READ SPECIFY SECTION ROUTINES
 */

/*
 * read and build d.s for specify section items
 * expects specify to have been read and reads endspecify
 * current approach concatenates all specify sections in one mod.
 */
extern int __rd_spfy(struct mod_t *mdp)
{
 int tmpi, sav_decl_obj;
 unsigned tchktyp;
 char s1[RECLEN];
 
 __path_num = __tchk_num = 1;
 sav_decl_obj = __cur_declobj;
 __cur_declobj = SPECIFY;
 /* all specify sections concatenated together */
 if (mdp->mspfy == NULL) mdp->mspfy = alloc_spfy();
 __cur_spfy = mdp->mspfy;
 /* at this point only module symbol tabl on scope stack since specify */
 /* is module item */
 if (__top_sti != 0) __misc_terr(__FILE__, __LINE__);
 /* place special symbol table for specparams on scope stack */
 __venviron[++__top_sti] = __cur_spfy->spfsyms;

 for (;;)
  {
   __get_vtok();
   switch ((byte) __toktyp)
    {
     case SPECPARAM: 
      if (!rd_specparamdecl())
       {
        /* notice unless T (found ;) will not sync to normal ( path */
specitem_resync:
        switch ((byte) __syncto_class) {
         case SYNC_FLEVEL: case SYNC_MODLEVEL: case SYNC_STMT:
          __top_sti--;
          return(FALSE);
         case SYNC_SPECITEM:
          break;
 	 /* if sync. to statement assume initial left out */
         default: __case_terr(__FILE__, __LINE__);
        }
       }
      continue;
     case IF: case LPAR: case IFNONE:
      if (!rd_delay_pth()) goto specitem_resync;
      break;
     case ENDSPECIFY: goto done;
     case ID:
      /* this must be timing check */
      if (*__token == '$')
       {
        if ((tmpi = __fr_tcnam(__token)) == -1)
         {
          __pv_ferr(1228,
           "system task %s illegal in specify section", __token);
	  goto err_skip;
         }
        tchktyp = tmpi;
        __get_vtok();
        if (__toktyp != LPAR)
	 {
	  __pv_ferr(1231,
          "timing check %s argument list left parenthesis expected - %s read",
  	   __to_tcnam(s1, tchktyp) , __prt_vtok());
          goto err_skip;
	 } 
        /* specify system timing check tasks */
	/* the routines fill cur tchk */
        switch ((byte) tchktyp) {
         case TCHK_SETUP: case TCHK_HOLD:
          if (!rd_setup_or_hold_tchk(tchktyp)) goto specitem_resync;
          break;
         case TCHK_SETUPHOLD:
          if (!rd_setuphold_tchk()) goto specitem_resync;
          break;
         case TCHK_WIDTH:
          if (!rd_width_tchk()) goto specitem_resync;
          break;
         case TCHK_PERIOD:
          if (!rd_period_tchk()) goto specitem_resync;
          break;
         case TCHK_SKEW: case TCHK_RECOVERY: case TCHK_REMOVAL:
          if (!rd_skew_recov_rem_tchk(tchktyp)) goto specitem_resync;
          break;
         case TCHK_RECREM:
          if (!rd_recrem_tchk()) goto specitem_resync;
          break;
         case TCHK_NOCHANGE:
          if (!rd_nochg_tchk()) goto specitem_resync;
          break;
         case TCHK_FULLSKEW:
         case TCHK_TIMESKEW:
          /* SJM 11/21/03 - this and other new 2001 tchks not supported */
          /* for now just skip and later add support for this and others */
          __pv_fwarn(3124,"%s timing check not yet supported - ignored",
           __to_tcnam(__xs, tchktyp)); 
          goto err_skip;
         default: __case_terr(__FILE__, __LINE__);
        }
        /* add to timing check list */
	if (__end_tchks == NULL) __cur_spfy->tchks = __cur_tchk;
	else __end_tchks->tchknxt = __cur_tchk;
	__end_tchks = __cur_tchk;
        break;
       }
      /* fall through to error since ID not specify item */
      /*FALLTHRU*/
     default:
      __pv_ferr(1229, "specify section item expected - %s read",
       __prt_vtok());
err_skip:
     if (!__spec_vskipto_any(SEMI)) goto specitem_resync;
     /* just fall through if seme to next */
    }
  }
done:
 __top_sti = 0;
 __cur_declobj = sav_decl_obj;
 /* notice freezing at module end for specparam symbol table too */
 return(TRUE);
}

/*
 * allocate a specify block - called when first specify block seen
 */
static struct spfy_t *alloc_spfy(void)
{
 struct spfy_t *spcp;

 spcp = (struct spfy_t *) __my_malloc(sizeof(struct spfy_t));
 /* notice this symbol table links to mod through this special specify */
 /* block but otherwise no symbol table links */
 spcp->spfsyms = __alloc_symtab(TRUE);
 spcp->spcpths = NULL; 
 spcp->tchks = NULL;
 spcp->msprms = NULL; 
 spcp->sprmnum = 0;
 __end_spcpths = NULL;
 __end_tchks = NULL;
 __end_msprms = NULL;
 return(spcp);
}

/*
 * read the specparam declaration statement 
 * form: specparam [name] = [constant?], ...
 * no  # and () around delay is optional in parameter decl.
 * reads parameter name through ending ;
 *
 * here width actual bit width - later converted to int or real
 * and then usually to 64 bit delay
 */
static int rd_specparamdecl(void)
{
 int good, sav_slin_cnt, sav_sfnam_ind, prng_decl, pwid, rwid, r1, r2;
 struct net_t *np;
 struct expr_t *dx1, *dx2, *x1, *x2;
 char paramnam[IDLEN];

 pwid = 0;
 dx1 = dx2 = x1 = x2 = NULL;
 prng_decl = FALSE;
 __get_vtok();
 if (__toktyp == LSB)
  {
   /* also check to make sure ranges are non x/z 32 bit values */
   if (!__rd_opt_param_vec_rng(&dx1, &dx2)) return(FALSE);
   if (dx1 == NULL || dx2 == NULL) goto rd_param_list;

   r1 = (int) __contab[dx1->ru.xvi];
   r2 = (int) __contab[dx2->ru.xvi];
   pwid = (r1 >= r2) ? r1 - r2 + 1 : r2 - r1 + 1; 
   x1 = dx1; x2 = dx2;
   prng_decl = TRUE;
   /* know parameter name read */
  }

rd_param_list:
 for (;;)	
  {
   if (__toktyp != ID)
    {
     __pv_ferr(1230, "specparam name expected - %s read", __prt_kywrd_vtok());
bad_end:
     /* part of delay expression may have been built */
     if (!__spec_vskipto2_any(COMMA, SEMI)) return(FALSE);
     if (__toktyp == COMMA) { __get_vtok(); continue; }
     return(TRUE);
    }
   strcpy(paramnam, __token);

   /* notice the initial value is required */
   __get_vtok();
   if (__toktyp != EQ)
    {
     __pv_ferr(1232,
      "specparam declaration equal expected - %s read", __prt_vtok());
     goto bad_end;
    }
   /* 06/22/00 - SJM - special path pulse form, for now ignore with warn */
   if (strncmp(paramnam, "PATHPULSE$", 10) == 0)
    {
     __pv_fwarn(3102,
      "%s special path pulse specparam ignored - use +show_canceled_e option instead",
      paramnam);
     /* 06/22/00 - SJM - FIXME - need to really parse this */ 
     if (!__spec_vskipto_any(SEMI)) 
      {
       __pv_ferr(3408,
        "PATHPULSE$ specparam %s right hand side value illegal format",
        paramnam);
       return(FALSE);
      }
     goto nxt_sparam;
    }

   /* know symbol table env. in specify specparam rhs specparam local */
   __get_vtok();
   __sav_spsytp = __venviron[0];
   __venviron[0] = __venviron[1];
   __top_sti = 0;
   good = __col_paramrhsexpr();
   __venviron[0] = __sav_spsytp;
   __top_sti = 1; 
   /* on error, does not add spec param */
   if (!good) goto bad_end;
   __bld_xtree(0);

   /* checking rhs does no evaluation (not known yet) but set sizes */
   /* and makes sure contains only num and previously defined specparams */
   if (__expr_has_glb(__root_ndp)
    || !__src_rd_chk_paramexpr(__root_ndp, WBITS))
    {
     __pv_ferr(1233,
      "specparam %s right hand side %s illegal - defined specparams and constants only",
      paramnam, __msgexpr_tostr(__xs, __root_ndp));
     goto nxt_sparam;
    }

   /* SJM 06/17/99 - illegal to assign string literal to specparam */
   /* SJM 02/04/00 - must allow since needed for models where PLI used */
   /* to read parameter value - and should be this is just wide reg */
   /* --- REMOVED --- 
   if (__root_ndp->is_string)
    {
     __pv_fwarn(659,
      "specparam %s right hand side string not a delay value - converted to 0 delay",
      paramnam);
     -* need to still add value of x to prevent further errors *-
     __free2_xtree(__root_ndp);
     __root_ndp->szu.xclen = WBITS;
     __set_numval(__root_ndp, 0L, 0L, WBITS);  
     -* notice x1, x2 range expressions always WBITS-1 to 0 for specparams *-  
    }
   --- */

   /* if range declared that is used, else if non scalar expr, use that */
   if (prng_decl)
    {
     if (pwid == 1) x1 = x2 = NULL;
     else
      {
       /* for specparam - assume int/unsigned, convert to real if needed */
       x1 = __bld_rng_numxpr(pwid - 1L, 0L, WBITS);
       x2 = __bld_rng_numxpr(0L, 0L, WBITS);
      }
    }
   else
    {
     if (__root_ndp->is_real) rwid = REALBITS; 
     else rwid = __root_ndp->szu.xclen;

     if (rwid == 1) x1 = x2 = NULL;
     else
      {
       /* if expr value unsized, know will be 32 bit width already */  
       x1 = __bld_rng_numxpr(rwid - 1, 0, WBITS);
       x2 = __bld_rng_numxpr(0, 0, WBITS);
      }
    }

   /* check and links on modules parameter list */
   /* when rhs expr. evaluated, if real will change */
   /* LOOKATME - problem with all params in list sharing range xprs? */ 
   np = __add_param(paramnam, x1, x2);
 
   /* using ncomp delay union to store original expresssion - set first */
   /* need this separate copy even after parameter value assigned */
   np->nu.ct->n_dels_u.d1x = __root_ndp;
   np->nu.ct->parm_srep = SR_PXPR;

   /* can assign specparam value immediately */
   /* SJM 06/17/99 - needs to run in run/fixup mode - statement loc set */
   sav_slin_cnt = __slin_cnt;
   sav_sfnam_ind = __sfnam_ind;
   __sfnam_ind = (int) np->nsym->syfnam_ind;
   __slin_cnt = np->nsym->sylin_cnt;

   assign_1specparam(np, __root_ndp, prng_decl, pwid);

   __slin_cnt = sav_slin_cnt;
   __sfnam_ind = sav_sfnam_ind;

   /* specparams can never be strings or IS forms */ 
   __mod_specparams++;

nxt_sparam:
   if (__toktyp == SEMI) break;
   if (__toktyp != COMMA)
    {
     __pv_ferr(1236,
      "specparam ; or , separator expected - %s read", __prt_vtok());
     if (!__spec_vskipto2_any(COMMA, SEMI)) return(FALSE); 
     if (__toktyp == SEMI) break;
    }
   __get_vtok();
  }
 return(TRUE);
}

/*
 * assign values to specparams - like defparams but can never be IS form
 *
 * 02/04/00 - SJM change to move toward v2k LRM and match XL better
 * type determined from RHS - range allowed and used for width
 *
 * FIXME - still for signed negatives need to convert to real or will
 * not see negative in delay prep - how fix this 
 */
static void assign_1specparam(struct net_t *np, struct expr_t *ndp,
 int prng_decl, int pwid)
{
 int wlen;
 word *wp;
 struct xstk_t *xsp;

 /* need dummy itree place on itree stack for eval */
 xsp = __eval_xpr(ndp);

 /* case 1: range declaration - may need to convert value */
 if (prng_decl)
  {
   /* FIXME - need to add signed keyword */
   /* convert declared param type real rhs to int/reg */
   if (ndp->is_real)
    {
     __cnv_stk_fromreal_toreg32(xsp);
     np->nu.ct->pbase = BDEC;
     np->nu.ct->prngdecl = TRUE;
     np->nwid = xsp->xslen;
     np->ntyp = N_REG;
     np->n_signed = TRUE;
    }
   else
    {
     /* xsp always right width for parameter net width */
     if (xsp->xslen != pwid) __sizchgxs(xsp, pwid);

     np->nu.ct->prngdecl = TRUE;
     np->nwid = xsp->xslen;
     np->ntyp = N_REG;
     if (np->nwid > 1) { np->n_isavec = TRUE; np->vec_scalared = TRUE; } 
     if (ndp->has_sign) np->n_signed = TRUE;
     np->nu.ct->pbase = ndp->ibase;
    }

   wlen = wlen_(np->nwid);
   wp = (word *) __my_malloc(2*WRDBYTES*wlen); 
   memcpy(wp, xsp->ap, 2*WRDBYTES*wlen);
   __pop_xstk();
   np->nva.wp = wp;
   np->srep = SR_PNUM;
   return;
  }

 /* FIXME - for now because no signed keyword, need to make signed int */
 /* FIXME - this must be wrong */
 /* into real if negative so negative values will work in timing checks */
 if (!ndp->is_real && xsp->xslen == WBITS)
  {
   /* if signed int and negative need to convert to real */
   if (ndp->has_sign && xsp->bp[0] == 0L
    && ((xsp->ap[0] & ~__masktab[31]) != 0L))
    {
     /* will not fit in int, convert to real (always signed) */
     __cnv_stk_fromreg_toreal(xsp, (ndp->has_sign == 1));
     ndp->is_real = TRUE;
     ndp->has_sign = TRUE;
     ndp->szu.xclen = REALBITS;
    }
   }

 /* case 2: type determined from constant expr */
 /* spec params either reg or real - by here if range decl ndp fixed */
 if (ndp->is_real)
  {
   np->nwid = REALBITS;
   np->ntyp = N_REAL;
   np->n_signed = TRUE;
   np->n_isavec = TRUE;
   np->nu.ct->pbase = BDBLE;
   wp = (word *) __my_malloc(2*WRDBYTES); 
   memcpy(wp, xsp->ap, 2*WRDBYTES);
  }
 else
  {
   np->nwid = xsp->xslen;
   np->ntyp = N_REG;
   if (np->nwid > 1) { np->n_isavec = TRUE; np->vec_scalared = TRUE; } 
   if (ndp->has_sign) np->n_signed = TRUE;
   if (ndp->is_string) np->nu.ct->pstring = TRUE;

   np->nu.ct->pbase = ndp->ibase;
   wlen = wlen_(np->nwid);
   wp = (word *) __my_malloc(2*WRDBYTES*wlen); 
   memcpy(wp, xsp->ap, 2*WRDBYTES*wlen);
  }
 __pop_xstk();
 /* build wire value - this is assign to wire so wire flags unchged */
 np->nva.wp = wp;
 np->srep = SR_PNUM;
}

/*
 * DELAY PATH ROUTINES
 */

/*
 * read the ([path desc. list?] [=*]> [path desc. list?]) = [path val.]
 * know initial ( read and if conditional present in condx
 * notice no path if any error but may still return T
 *
 * here when collecting expressions both specparams and top module symbol
 * table accessible for wires but only local specparams for delay
 */
static int rd_delay_pth(void)
{
 int good;
 int pdtyp, pth_edge, pthpolar, datasrcpolar, is_ifnone;
 struct sy_t *pthsyp;
 struct exprlst_t *ilstx, *olstx;
 struct paramlst_t *pmphdr;
 struct expr_t *condx, *datsrcx, *lop, *last_dsx;
 struct spcpth_t *pthp;

 is_ifnone = FALSE;
 condx = datsrcx = NULL;
 /* needed since gcc sees as unset - do not think so */
 olstx = NULL;
 pdtyp = PTH_NONE;
 datasrcpolar = POLAR_NONE;
 pthpolar = POLAR_NONE;
 if (__toktyp == IFNONE)
  { 
   __get_vtok();
   if (__toktyp != LPAR)
    {
     __pv_ferr(1197,
      "sdpd ifnone token not followed by path beginning ( - %s read",
      __prt_vtok());
     /* here skipping to ) will skip path - no start tok - can not correct */
     if (!__spec_vskipto_any(SEMI)) return(FALSE);
     return(TRUE);
    }
   is_ifnone = TRUE;
   goto no_pthcond;
  }
 if (__toktyp == LPAR) goto no_pthcond;
 /* must be if token to get here, read optional condition expr. 1st */
 __get_vtok();
 if (__toktyp != LPAR)
  {
   __pv_ferr(1251,
    "sdpd conditional expression if token not followed by ( - %s read",
     __prt_vtok());
bad_sdp:
   if (!__spec_vskipto2_any(SEMI, RPAR)) return(FALSE);
   if (__toktyp == SEMI) return(TRUE);
   /* have ) make cond x NULL - this is not delay this is if actual expr */
   __set_numval(__exprtab[0], 1L, 1L, 1);
   __last_xtk = 0;
   goto bad_sdpx;
  }
 __get_vtok();
 if (!__col_parenexpr(-1)) goto bad_sdp;
bad_sdpx:
 __bld_xtree(0);
 condx = __root_ndp;
 if (__expr_has_glb(condx))
  {
   __pv_ferr(1022,
    "global hierarchical reference illegal in state dependent path condition %s",
    __msgexpr_tostr(__xs, condx));
  }
 __get_vtok();
 if (__toktyp != LPAR)
  {
   __pv_ferr(1252,
    "sdpd conditional expression not followed by path start ( - %s read",
    __prt_vtok());
   /* here skipping to ) will skip path */
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
   return(TRUE);
  }

no_pthcond:
 /* for edge sensitive path can have edge before input */
 /* but only pos and neg edges */
 __get_vtok();
 if (__toktyp == NEGEDGE || __toktyp == POSEDGE) 
  {
   pth_edge = (__toktyp == NEGEDGE) ? E_NEGEDGE : E_POSEDGE;
   __get_vtok();
  }
 else pth_edge = NOEDGE;

 /* this requires read 1st token of path */ 
 if ((ilstx = rd_pthtermlst()) == NULL)
  {
bad_pth:
   if (!__spec_vskipto3_any(SEMI, RPAR, EQ)) return(FALSE);
   if (__toktyp == RPAR) goto get_eq; 
   if (__toktyp == EQ) goto got_eq;
   return(TRUE);
  }
 /* this just attempts to catch some common errors */
 if (__toktyp == RPAR || __toktyp == TCHKEVAND)
  {
   __pv_ferr(1253,
    "input path description connector operator or semicolon expected - %s read",
    __prt_vtok());
   goto bad_pth;
  }
 /* path polarity is option stored for pli but not used */
 if (__toktyp == PLUS || __toktyp == MINUS)
  {
   pthpolar = (__toktyp == PLUS) ? POLAR_PLUS : POLAR_MINUS;
   __get_vtok();
  }
 else pthpolar = POLAR_NONE;

 /* path type required */
 if (__toktyp == FPTHCON) pdtyp = PTH_FULL; 
 else if (__toktyp == PPTHCON) pdtyp = PTH_PAR;
 else
  {
   __pv_ferr(1258,
    "either full => or parallel *> path operator expected - %s read",
    __prt_vtok());
   goto bad_pth;
  }
 /* if ( here then form is ([dst. term list] [polarity?]:[data src. expr.]) */
 __get_vtok();
 if (__toktyp == LPAR)
  { 
   /* this requires read 1st token of path */ 
   __get_vtok();
   if ((olstx = rd_pthtermlst()) == NULL) goto bad_end;
   /* data source polarity determines delay selection */
   if (__toktyp == PLUS || __toktyp == MINUS)
    {
     datasrcpolar = (__toktyp == PLUS) ? POLAR_PLUS : POLAR_MINUS;
     __get_vtok();
    }
   else datasrcpolar = POLAR_NONE;
   if (__toktyp != COLON)  
    {
     __pv_ferr(1254,
      "data source path destination colon terminator expected - %s read",
      __prt_vtok());
     goto bad_pth;
    }
   __get_vtok();
   /* comma separated list allowed here - width must match dest. width */
   if (!__col_parenexpr(-1)) goto bad_pth;
   __bld_xtree(0);
   /* common edge path trigger as 1 expression not list case */
   if (__toktyp != COMMA)
    { datsrcx = __root_ndp; __get_vtok(); goto chk_endrpar; }

   /* this is complicated data source expression list case */
   lop = __alloc_newxnd();
   lop->optyp = FCCOM;
   lop->ru.x = NULL;
   lop->lu.x = __root_ndp;
   datsrcx = lop;
   for (last_dsx = lop;;) 
    {
     /* if good this reads trailing delimiter */
     if (!__col_connexpr(-1)) goto bad_end;
     __bld_xtree(0);
     lop = __alloc_newxnd();
     lop->optyp = FCCOM;
     lop->ru.x = NULL;
     lop->lu.x = __root_ndp;
     last_dsx->ru.x = lop;
     if (__toktyp == RPAR) { __get_vtok(); break; }
     if (__toktyp != COMMA) 
      {
       __pv_ferr(1255,
        "edge sensitive path data source expression list separator expected - %s read",
        __prt_vtok());
       goto bad_end;
      }
     __get_vtok();
     if (__toktyp == COMMA || __toktyp == RPAR) 
      {
        __pv_ferr(1259,
         "edge sensitive path data source expression ,, or ,) forms illegal");
        goto bad_end;
      }
    }
  }
 else if ((olstx = rd_pthtermlst()) == NULL) goto bad_pth;

chk_endrpar:
 if (__toktyp != RPAR)
  {
   __pv_ferr(1256,
    "path output terminal list right parenthesis expected - %s read",
    __prt_vtok());
   goto bad_pth;
  }
get_eq:
 __get_vtok();
 if (__toktyp != EQ) 
  {
   __pv_ferr(1257, "path delay equal sign expected - %s read",
    __prt_vtok());
bad_end:
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
   return(TRUE);
  }
got_eq:
 /* know symbol table env. in specify always specparam local on mod. lev. */
 __sav_spsytp = __venviron[0];
 __venviron[0] = __venviron[1];
 __top_sti = 0;
 /* notice = read but not ( - unusual case of no 1st token read before call */ 
 good = rd_pathdelaylist(&pmphdr);
 __venviron[0] = __sav_spsytp;
 __top_sti++; 
 if (!good) goto bad_end;

 /* build the path delay element */
 pthp = (struct spcpth_t *) __my_malloc(sizeof(struct spcpth_t));
 init_spcpth(pthp);
 pthp->pthtyp = pdtyp; 
 pthp->pthpolar = pthpolar; 
 pthp->peins = (struct pathel_t *) ilstx;
 pthp->peouts = (struct pathel_t *) olstx;

 /* add the location identifying symbol */
 pthsyp = __bld_loc_symbol(__path_num, __venviron[0], "path", "delay path");
 pthsyp->sytyp = SYM_PTH;
 pthsyp->syfnam_ind = __cur_fnam_ind;
 pthsyp->sylin_cnt = __lin_cnt;
 pthp->pthsym = pthsyp;
 pthsyp->el.epthp = pthp;
 pthsyp->sydecl = TRUE;
 __path_num++;

 /* set delay part */
 pthp->pth_delrep = DT_CMPLST;
 pthp->pth_du.pdels = pmphdr;

 /* set sdpd and conditional path values */ 
 pthp->pthedge = pth_edge;  
 pthp->dsrc_polar = datasrcpolar;
 pthp->datasrcx = datsrcx;
 pthp->pth_ifnone = is_ifnone;
 pthp->pthcondx = condx;

 if (__end_spcpths == NULL) __cur_spfy->spcpths = pthp;
 else __end_spcpths->spcpthnxt = pthp;
 __end_spcpths = pthp;
 return(TRUE);
}

/*
 * read a path terminal list
 * know 1st token read and reads ending )
 */
static struct exprlst_t *rd_pthtermlst(void)
{
 struct exprlst_t *xpmphdr, *xpmp, *last_xpmp;

 /* build specify terminal list - semantics that requires subset of mod. */
 /* I/O port terminals checked later */
 /* no ,, and at least one required */
 for (xpmphdr = NULL, last_xpmp = NULL;;)
  {
   /* this will have skipped to end of statement on error */
   if (!col_pthexpr()) return(NULL);
   __bld_xtree(0);
   xpmp = __alloc_xprlst();
   xpmp->xp = __root_ndp;
   if (last_xpmp == NULL) xpmphdr = xpmp; else last_xpmp->xpnxt = xpmp;
   last_xpmp = xpmp;
   if (__toktyp == RPAR || __toktyp == FPTHCON || __toktyp == PPTHCON
    || __toktyp == PLUS || __toktyp == MINUS || __toktyp == TCHKEVAND
    || __toktyp == COLON) break;
   __get_vtok();
  }
 return(xpmphdr);
}

/*
 * collect a delay path terminal expression
 * expects 1st token to be read and read ending token
 * ends with ) or ',' or => or *> or - or + or : preceding connection op.
 * for timing checks can end with TCHKEVAND &&&
 * allows full expressions because port bit select can be full const. expr.
 *
 * notice ending thing not included in expr. but left in token
 *
 * this collects a specify expr. - caller must eliminate wrong tokens for
 * either tchk or path 
 *
 * notice this is lhs element not delay constants
 * maybe should be empty on error
 */
static int col_pthexpr(void)
{
 int parlevel, sqblevel;

 for (__last_xtk = -1, parlevel = 0, sqblevel = 0;;)
  {
   switch ((byte) __toktyp) {
    case LPAR: parlevel++; break;
    /* any expression (must later be constant) legal in selects */
    case LSB: sqblevel++; break;
    case RPAR: 
     if (parlevel <= 0 && sqblevel <= 0) return(TRUE); else parlevel--;
     break;
   case RSB:
    sqblevel--;
    break;
   case COMMA:
    /* illegal here but parse and check later */
    if (parlevel <= 0 && sqblevel <= 0) return(TRUE);
    break;
   case PLUS: case MINUS:
    if (parlevel <= 0 && sqblevel <= 0) return(TRUE);
    break;
   case COLON:
    if (parlevel <= 0 && sqblevel <= 0) return(TRUE);
    break;
   /* notice these are never nested */
   case FPTHCON: case PPTHCON: case TCHKEVAND:
    return(TRUE);
   case TEOF:
   case SEMI:
    goto bad_end;
   }
   if (!__bld_expnode()) goto bad_end;
   __get_vtok();
  }

bad_end:
 __set_xtab_errval();
 return(FALSE);
}

/*
 * read delay path list 
 * almost same as read oparams del but surrounding () always optional
 *
 * reads and checks for ending ; 
 * builds a parameter/delay list and returns pointer to header
 * returns F on sync error - caller must resync
 * but in places with known delimiter attempt to resync to delim 
 */
static int rd_pathdelaylist(struct paramlst_t **pmphdr)
{
 int rv;
 struct paramlst_t *pmp, *last_pmp;

 *pmphdr = NULL;

 /* this is #[number] or #id - not (..) form - min:typ:max requires () */
 /* for path delay will never see this form */
 __get_vtok();
 /* case 1: has optional () surround list */
 if (__toktyp == LPAR) 
  {
   for (last_pmp = NULL;;)
    {
     __get_vtok();
     if (!__col_delexpr())
      {
       /* need to look to skip to any possible end ( may be unmatched */
       if (!__vskipto3_modend(COMMA, RPAR, SEMI)) return(FALSE);
       if (__toktyp == SEMI) return(FALSE);
       if (__toktyp == RPAR) { __get_vtok(); rv = FALSE; goto chk_semi; }
       /* if comman do not add but continue checking */
       continue;
      }       
     __bld_xtree(0);
     pmp = __alloc_pval();
     pmp->plxndp = __root_ndp;

     /* link on front */
     if (last_pmp == NULL) *pmphdr = pmp; else last_pmp->pmlnxt = pmp;
     last_pmp = pmp;

     if (__toktyp == COMMA) continue;
     if (__toktyp == RPAR) break;
     /* should never happen - sync on err above, if does give up */
     __pv_ferr(1050,
      "path delay delay list comma or right parenthesis expected - %s read",
      __prt_vtok());
     return(FALSE);
    }
   rv = TRUE;
   __get_vtok();
chk_semi:
   if (__toktyp != SEMI)
    {
     __pv_ferr(1279, "path delay final ; expected - %s read", __prt_vtok());
     return(FALSE);
    }
   return(rv);
  }
 /* case 2: optional surrounding omitted */ 
 for (last_pmp = NULL;;)
  {
   /* this reads the end , or ; */
   if (!__col_paramrhsexpr())
    {
     /* need to look to skip to any possible end ( may be unmatched */
     if (!__vskipto3_modend(COMMA, RPAR, SEMI)) return(FALSE);
     if (__toktyp == SEMI) return(FALSE);
     if (__toktyp == RPAR) { __get_vtok(); rv = FALSE; goto chk_semi; }
     __get_vtok();
     continue;
    }       
   __bld_xtree(0);
   pmp = __alloc_pval();
   pmp->plxndp = __root_ndp;
   if (last_pmp == NULL) *pmphdr = pmp; else last_pmp->pmlnxt = pmp;
   last_pmp = pmp;

   if (__toktyp == COMMA) { __get_vtok(); continue; }
   if (__toktyp == SEMI) break;
   /* should never happen - sync on err above, if does give up */
   __pv_ferr(1050,
    "path delay delay list comma or semicolon expected - %s read",
     __prt_vtok());
   return(FALSE);
  }
 return(TRUE);
}
 
/*
 * initialize a specify section delay path
 */
static void init_spcpth(struct spcpth_t *pthp)
{
 pthp->pthtyp = PTH_NONE;
 pthp->pth_gone = FALSE;
 pthp->pth_as_xprs = TRUE;
 pthp->pth_delrep = DT_NONE;
 pthp->pthpolar = FALSE;
 pthp->pthedge = NOEDGE;
 pthp->dsrc_polar = POLAR_NONE; 
 pthp->pth_ifnone = FALSE;
 pthp->pth_0del_rem = FALSE;
 pthp->pthsym = NULL;
 pthp->last_pein = -1;
 pthp->last_peout = -1;
 pthp->peins = NULL;
 pthp->peouts = NULL;
 pthp->pth_du.d1v = NULL;
 pthp->datasrcx = NULL;
 pthp->pthcondx = NULL;
 pthp->spcpthnxt = NULL;
}

/*
 * TIMING CHECK READ ROUTINES
 */

/*
 * read setup or hold timing check
 * know system task keyword and ( read and reads ending ; if possible
 *
 * read and parse timing checks as is - during prep changed to needed form
 */
static int rd_setup_or_hold_tchk(unsigned ttyp)
{
 int fnum, lnum;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;
 struct tchk_t tmptchk;
 struct expr_t *limx;

 __init_tchk(&tmptchk, ttyp);
 /* must save location since, need system task line as tchk loc. */
 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;
 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 /* notice can only be ID if present */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1261, "%s timing check does not end with ); - %s read",
    __to_tcnam(__xs, ttyp), __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 
 /* add the location idnentifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 tcsyp->el.etcp = __cur_tchk;
 tcsyp->sydecl = TRUE;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;

 __cur_tchk->ntfy_np = (struct net_t *) syp;
 /* setup and hold identical - users changes order of args - ref. event */
 /* always first so $setup(data, clk, ...), $hold(clk, data, ...) */
 pmp = __alloc_pval(); pmp->plxndp = limx; pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 return(TRUE);
} 

/*
 * build timing check symbol
 *
 * even if already declared, change to delay object
 * if used previously, change to delay object - error if previously declared
 * if used in previous expr. but later declared, error emitted at declaration
 * common case first used in $set[it]delay expr. and added to symbol table
 * as wire then here changed to delay object
 */
extern struct sy_t *__bld_loc_symbol(int num, struct symtab_t *sytp,
 char *pref, char *emsgnam)
{
 char s1[RECLEN];
 struct sy_t *syp;

 sprintf(s1, "__%s$$%d", pref, num);
 syp = __decl_sym(s1, sytp);
 if (!__sym_is_new)
  {
   if (syp->sydecl)
    {
     __pv_ferr(1160,
      "%s constructed internal name %s conflicts with declared %s",
      emsgnam, s1, __to_sytyp(__xs, syp->sytyp));
    }
   else
    {
     __pv_ferr(1160, "%s constructed internal name %s conflicts with wire",
      emsgnam, s1);
    }
  }
 return(syp);
}

/*
 * read the event and first limit part of all timing checks
 * common code for all timing checks, limits differ
 * this must read 1st token before reading tchk events
 * if returns F, parameters not set
 * this syncs to ; or item location and returns F 
 * on T reads ) or , after first (maybe only) limit
 * notice 1 limit always required
 */
static int rd_tchk_part(unsigned ttyp, struct tchk_t *tcp,
 struct expr_t **limx)
{
 struct expr_t *xp, *condx;
 int edgval;

 /* notice ref. always first */
 __get_vtok();
 if (!rd_tchk_selector(&edgval, &xp, &condx))
  {
   if (!__spec_vskipto2_any(SEMI, COMMA)) return(FALSE);
   if (__toktyp == SEMI)
    {
got_semi:
     __syncto_class = SYNC_SPECITEM;
     return(FALSE);
    }
  }
 tcp->startedge = edgval;  
 tcp->startxp = xp;
 tcp->startcondx = condx;

 /* tcp initialized to empty fields */
 if (ttyp != TCHK_WIDTH && ttyp != TCHK_PERIOD) 
  {
   __get_vtok();
   if (!rd_tchk_selector(&edgval, &xp, &condx))
    {
     if (!__spec_vskipto2_any(SEMI, COMMA)) return(FALSE);
     if (__toktyp == SEMI) goto got_semi;
    }  
   tcp->chkedge = edgval;  
   tcp->chkxp = xp;
   tcp->chkcondx = condx;
  }
 __get_vtok();
 __sav_spsytp = __venviron[0];
 __venviron[0] = __cur_spfy->spfsyms;
 /* limit is one number but can be d:d:d form - but know ends with , or ) */
 if (!__col_delexpr())
  { 
   if (!__spec_vskipto2_any(SEMI, COMMA))
    { __venviron[0] = __sav_spsytp; return(FALSE); }
   if (__toktyp == SEMI)
    { __venviron[0] = __sav_spsytp; goto got_semi; }
   /* set error, if ,, form will not get here */
   /* make it look like ,, form */
   __set_0tab_errval();
  }
 __bld_xtree(0);
 if (__has_top_mtm)
  {
   __pv_fwarn(653,
    "%s timing check min:typ:max limit expression needs parentheses under 1364 - unportable",
    __to_tcnam(__xs, ttyp));
  }
 *limx = __root_ndp;
 __venviron[0] = __sav_spsytp;
 return(TRUE);
}

/*
 * initialize a timing check record
 */
extern void __init_tchk(struct tchk_t *tcp, unsigned ttyp)
{
 tcp->tchktyp = ttyp; 
 /* notice del rep applies to both limits if present */
 tcp->tc_delrep = DT_CMPLST; 
 tcp->tc_delrep2 = DT_CMPLST; 
 tcp->tc_gone = FALSE;
 tcp->tc_supofsuphld = FALSE;
 tcp->tc_recofrecrem = FALSE;
 tcp->tc_haslim2 = FALSE;
 tcp->startedge = NOEDGE;
 tcp->startxp = NULL;
 tcp->tcsym = NULL;
 tcp->startcondx = NULL;
 tcp->chkedge = NOEDGE;
 tcp->chkxp = NULL;
 tcp->chkcondx = NULL;
 /* notice this may be nil */
 tcp->ntfy_np = NULL;
 tcp->tclim_du.pdels = NULL;
 tcp->tclim2_du.pdels = NULL;
 tcp->tchknxt = NULL;
}

/*
 * read setuphold timing check (both with 2 limits)
 * know system task keyword read
 */
static int rd_setuphold_tchk(void)
{
 unsigned ttyp;
 int fnum, lnum;
 struct tchk_t tmptchk;
 struct expr_t *limx, *lim2x;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;

 ttyp = TCHK_SETUPHOLD;
 __init_tchk(&tmptchk, ttyp);
 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;

 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 if (__toktyp != COMMA)
  {
   __pv_ferr(1267,
    "$setuphold hold limit expression , terminator expected - %s read",
    __prt_vtok());
   __spec_vskipto_any(SEMI);
   return(FALSE);
  }
 __get_vtok();
 __sav_spsytp = __venviron[0];
 __venviron[0] = __cur_spfy->spfsyms;
 /* can be ,, or ,) empty but required */
 if (!__col_delexpr())
  { 
   if (!__spec_vskipto2_any(SEMI, COMMA))
    { __venviron[0] = __sav_spsytp; return(FALSE); }
   if (__toktyp == SEMI)
    {
     __venviron[0] = __sav_spsytp;
     __syncto_class = SYNC_SPECITEM;
     return(FALSE);
    }
   /* set rhs error expr. */
   __set_0tab_errval();
  }
 __bld_xtree(0);
 if (__has_top_mtm)
  {
   __pv_fwarn(653,
    "%s timing check min:typ:max 2nd limit expression needs parentheses under 1364 - unportable",
    __to_tcnam(__xs, ttyp));
  }
 lim2x = __root_ndp;
 __venviron[0] = __sav_spsytp;

 /* notice can only be ID if present */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1262, "$setuphold timing check does not end with ); - %s read",
    __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 

 /* add the location idnentifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;
 __cur_tchk->ntfy_np = (struct net_t *) syp;
 pmp = __alloc_pval();
 pmp->plxndp = limx;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 pmp = __alloc_pval();
 pmp->plxndp = lim2x;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim2_du.pdels = pmp;
 __cur_tchk->tc_haslim2 = TRUE;
 return(TRUE);
}

/*
 * read recrem timing check (both with 2 limits)
 * know system task keyword read
 *
 * SJM 01/16/04 - almost same as setup hold but 2001 LRM has extra stuff
 * that is not yet added
 */
static int rd_recrem_tchk(void)
{
 unsigned ttyp;
 int fnum, lnum;
 struct tchk_t tmptchk;
 struct expr_t *limx, *lim2x;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;
 char s1[RECLEN];

 ttyp = TCHK_RECREM;
 __init_tchk(&tmptchk, ttyp);
 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;

 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);

 /* for recrem both terminals must be edges */ 
 if (tmptchk.startedge == NOEDGE)
  {
   __pv_ferr(1260,
   "%s timing check first recovery reference event missing required edge",
   __to_tcnam(s1, ttyp));
  }
 if (tmptchk.chkedge == NOEDGE)
  {
   __pv_ferr(1260,
    "%s timing 2nd removal reference event missing required edge",
   __to_tcnam(s1, ttyp));
  }

 if (__toktyp != COMMA)
  {
   __pv_ferr(1267,
    "$recrem removal limit expression , terminator expected - %s read",
    __prt_vtok());
   __spec_vskipto_any(SEMI);
   return(FALSE);
  }
 __get_vtok();
 __sav_spsytp = __venviron[0];
 __venviron[0] = __cur_spfy->spfsyms;
 /* can be ,, or ,) empty but required */
 if (!__col_delexpr())
  { 
   if (!__spec_vskipto2_any(SEMI, COMMA))
    { __venviron[0] = __sav_spsytp; return(FALSE); }
   if (__toktyp == SEMI)
    {
     __venviron[0] = __sav_spsytp;
     __syncto_class = SYNC_SPECITEM;
     return(FALSE);
    }
   /* set rhs error expr. */
   __set_0tab_errval();
  }
 __bld_xtree(0);
 if (__has_top_mtm)
  {
   __pv_fwarn(653,
    "%s timing check min:typ:max 2nd limit expression needs parentheses under 1364 - unportable",
    __to_tcnam(__xs, ttyp));
  }
 lim2x = __root_ndp;
 __venviron[0] = __sav_spsytp;

 /* notice can only be ID if present */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1262, "$setuphold timing check does not end with ); - %s read",
    __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 

 /* add the location idnentifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;
 __cur_tchk->ntfy_np = (struct net_t *) syp;
 pmp = __alloc_pval();
 pmp->plxndp = limx;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 pmp = __alloc_pval();
 pmp->plxndp = lim2x;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim2_du.pdels = pmp;
 __cur_tchk->tc_haslim2 = TRUE;
 return(TRUE);
}

/*
 * read width timing check
 * know system task keyword read
 * width has 2 limits (but 2nd can be omitted), period has 1
 */
static int rd_width_tchk(void)
{
 unsigned ttyp;
 int fnum, lnum;
 struct tchk_t tmptchk;
 struct expr_t *limx, *lim2x;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;

 ttyp = TCHK_WIDTH;
 __init_tchk(&tmptchk, ttyp);

 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;

 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 if (tmptchk.startedge != E_NEGEDGE && tmptchk.startedge != E_POSEDGE)
  __pv_ferr(1266,
   "$width timing check event missing required negedge or posedge"); 
 /* 2nd limit optional and becomes NULL */
 if (__toktyp == RPAR) { syp = NULL; lim2x = NULL; goto done; }
 if (__toktyp != COMMA)
  {
   __pv_ferr(1268,
    "$width first limit expression , terminator expected - %s read",
    __prt_vtok());
   __spec_vskipto_any(SEMI);
   return(FALSE);
  }
 __get_vtok();
 __sav_spsytp = __venviron[0];
 __venviron[0] = __cur_spfy->spfsyms;
 /* lrm says no ,, or ,) forms for width */
 /* since ignored by sim value of 0 ok place holder and just ignored */
 if (!__col_delexpr())
  { 
   if (!__spec_vskipto2_any(SEMI, COMMA))
    { __venviron[0] = __sav_spsytp; return(FALSE); }
   if (__toktyp == SEMI)
    {
     __venviron[0] = __sav_spsytp;
     __syncto_class = SYNC_SPECITEM;
     return(FALSE);
    }
   /* make it look like ,, form */
   __set_0tab_errval();
  }
 __bld_xtree(0);
 lim2x = __root_ndp;
 __venviron[0] = __sav_spsytp;

 /* notice can only be ID and may be omited */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
done:
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1263, "$width timing check does not end with ); - %s read",
    __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }

 /* notice no data even here */
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 

 /* add the location idnentifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 tcsyp->sydecl = TRUE;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;

 __cur_tchk->ntfy_np = (struct net_t *) syp;
 pmp = __alloc_pval();
 pmp->plxndp = limx;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 /* always build 2nd limit as 0 if missing (during fix) so present */
 __cur_tchk->tc_haslim2 = TRUE;
 /* notice missing 2nd limit ok since only timing verifier threshold */
 /* that stops errors if pulse less than threshold */ 
 if (lim2x == NULL) lim2x = __bld_rng_numxpr(0L, 0L, WBITS);
 pmp = __alloc_pval();
 pmp->plxndp = lim2x;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim2_du.pdels = pmp;
 return(TRUE);
}

/*
 * read period timing check
 * know system task keyword read
 * period has 1 limit (required), width has 2
 */
static int rd_period_tchk(void)
{
 unsigned ttyp;
 int fnum, lnum;
 struct tchk_t tmptchk;
 struct expr_t *limx;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;

 ttyp = TCHK_PERIOD;
 __init_tchk(&tmptchk, ttyp);

 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;

 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 if (tmptchk.startedge == NOEDGE)
  __pv_ferr(1269, "$period timing check event missing required edge"); 
 /* notice can only be ID if present */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1264, "$period timing check does not end with ); - %s read",
    __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 

 /* add the location idnentifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 tcsyp->sydecl = TRUE;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;

 __cur_tchk->ntfy_np = (struct net_t *) syp;
 pmp = __alloc_pval();
 pmp->plxndp = limx;
 pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 return(TRUE);
}

/*
 * read skew or recovery timing check
 * know system task keyword read
 * different timing checks have identical arguments
 *
 * SJM 01/16/04 - added removal first terminal is the ref events that
 * needs to be an edge for both
 */
static int rd_skew_recov_rem_tchk(unsigned ttyp)
{
 int fnum, lnum;
 struct tchk_t tmptchk;
 struct expr_t *limx;
 struct sy_t *syp, *tcsyp;
 struct paramlst_t *pmp;
 char s1[RECLEN];

 __init_tchk(&tmptchk, ttyp);
 /* must save location since, need system task line as tchk loc. */
 fnum = __cur_fnam_ind;
 lnum = __lin_cnt;
 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 if (ttyp == TCHK_RECOVERY || ttyp == TCHK_REMOVAL)
  {
   if (tmptchk.startedge == NOEDGE)
    __pv_ferr(1260,
     "%s timing check first reference event missing required edge",
     __to_tcnam(s1, ttyp));
  }

 /* notice can only be ID if present */
 if (__toktyp == COMMA) { syp = rd_notifier(); __get_vtok(); }
 else syp = NULL;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;
 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1265, "%s timing check does not end with ); - %s read",
    __to_tcnam(s1, ttyp), __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __cur_tchk = (struct tchk_t *) __my_malloc(sizeof(struct tchk_t));
 *__cur_tchk = tmptchk; 

 /* add the location identifying symbol */
 tcsyp = __bld_loc_symbol(__tchk_num, __venviron[0], "tchk", "timing check");
 tcsyp->sytyp = SYM_TCHK;
 tcsyp->syfnam_ind = fnum;
 tcsyp->sylin_cnt = lnum;
 tcsyp->sydecl = TRUE;
 __cur_tchk->tcsym = tcsyp;
 tcsyp->el.etcp = __cur_tchk;
 __tchk_num++;

 __cur_tchk->ntfy_np = (struct net_t *) syp;
 pmp = __alloc_pval(); pmp->plxndp = limx; pmp->pmlnxt = NULL;
 __cur_tchk->tclim_du.pdels = pmp;
 return(TRUE);
}

/*
 * must correctly parse $nochange but ignore with warning
 * this does not build and data structure
 */
static int rd_nochg_tchk(void)
{
 unsigned ttyp;
 struct tchk_t tmptchk;
 struct expr_t *limx;
 char s1[RECLEN];

 ttyp = TCHK_NOCHANGE;
 __init_tchk(&tmptchk, ttyp);
 /* this reads the first limit but not second */
 if (!rd_tchk_part(ttyp, &tmptchk, &limx)) return(FALSE);
 if (tmptchk.startedge != E_NEGEDGE && tmptchk.startedge != E_POSEDGE)
  __pv_ferr(1271,
   "$nochange timing check first reference event missing negedge or posedge"); 
 __free_xtree(limx);
 __free_xtree(tmptchk.startxp);
 __free_xtree(tmptchk.startcondx);
 __free_xtree(tmptchk.chkxp);
 __free_xtree(tmptchk.chkcondx);

 if (__toktyp != COMMA)
  {
   __pv_ferr(1272,
    "$nochange second event - comma expected - %s read", __prt_vtok());
   __spec_vskipto_any(SEMI);
   return(FALSE);
  }
 __get_vtok();
 __sav_spsytp = __venviron[0];
 __venviron[0] = __cur_spfy->spfsyms;
 /* lrm says no ,, or ,) forms for nochange */
 if (!__col_delexpr())
  { 
   if (!__spec_vskipto2_any(SEMI, COMMA))
    { __venviron[0] = __sav_spsytp; return(FALSE); }
   if (__toktyp == SEMI)
    {
     __venviron[0] = __sav_spsytp;
     __syncto_class = SYNC_SPECITEM;
     return(FALSE);
    }
   /* make it look like ,, form */
   __set_0tab_errval();
  }
 /* this is needed for checking */
 __bld_xtree(0);
 __free_xtree(__root_ndp);
 __venviron[0] = __sav_spsytp;
 /* even even error end, still add since good */
 if (__toktyp != RPAR) goto noparsemi;

 __get_vtok();
 if (__toktyp != SEMI)
  {
noparsemi:
   __pv_ferr(1273, "%s timing check does not end with ); - %s read",
    __to_tcnam(s1, ttyp), __prt_vtok());
   if (!__spec_vskipto_any(SEMI)) return(FALSE);
  }
 __pv_fwarn(599, "$nochange timing check has no effect in simulation");
 return(TRUE);
}

/*
 * read a timing check event - know always followed by , 
 * know 1st token read and reads ending ,
 * returns NULL on error, caller must skip to ;
 * caller syncs - returns NULL if syntax error (will need syncing)
 */
static int rd_tchk_selector(int *edgval, struct expr_t **xp,
 struct expr_t **condx)
{
 *edgval = NOEDGE;
 switch ((byte) __toktyp) {  
  case NEGEDGE: *edgval = E_NEGEDGE; break; 
  case POSEDGE: *edgval = E_POSEDGE; break; 
  case EDGE:
   __get_vtok();
   if (__toktyp != LSB) 
    {
     __pv_ferr(1281, "timing check event edge list [ expected - %s read",
      __prt_vtok());
edge_sync:
     /* caller must synchronize - except try for enclosing ] */
     if (!__spec_vskipto_any(RSB)) return(FALSE);
     goto get_pthx;
    }
   if (!rd_edges(edgval)) goto edge_sync;
   break;
  default: goto no_edge;
 }

get_pthx:
 __get_vtok();
no_edge:
 if (!col_pthexpr()) return(FALSE);
 __bld_xtree(0);
 *xp = __root_ndp;
 if (__toktyp != COMMA)
  {
   if (__toktyp != TCHKEVAND)
    {
     __pv_ferr(1282,
      "timing check data or reference event, comma or &&& expected - %s read",
      __prt_vtok());
     return(FALSE);
    }
   /* read &&& expr. */
   __get_vtok();
   if (!__col_connexpr(-1)) return(FALSE);
   if (__toktyp != COMMA)
    {
     __pv_ferr(1283,
      "timing check event &&& expression comma expected - %s read",
      __prt_vtok());
     return(FALSE);
    }
   __bld_xtree(0);
   *condx = __root_ndp;
  }
 else *condx = NULL;
 return(TRUE);
}

/*
 * read an edge list
 * know [ read and reads trailing ]
 * if error tries to sync to , ], ), ;
 */
static int rd_edges(int *edge)
{
 char s1[RECLEN];
 byte etmp, e1;

 *edge = etmp = NOEDGE;
 __letendnum_state = TRUE;
 for (;;)
  {
   strcpy(s1, "");
   for (;;)
    {
     __get_vtok();
     switch ((byte) __toktyp) {
      case COMMA: case RSB: goto got_pair;
      case ID: strcat(s1, __token); break;
      case NUMBER: strcat(s1, __numtoken); break;
      default:
       __pv_ferr(1284, "edge list edge value pair expected - %s read",
        __prt_vtok());
       __letendnum_state = FALSE;
       return(FALSE);
      }
    }
got_pair:
   if (strlen(s1) > 2)
    {
bad_edge:
     __pv_ferr(1286, "edge list element %s illegal", s1);
     continue;
    }
   switch (s1[0]) {
    case '0':
     if (s1[1] == '1') e1 = EDGE01;
     else if (s1[1] == 'x') e1 = EDGE0X;
     else goto bad_edge;
     break;
    case '1':
     if (s1[1] == '0') e1 = EDGE10;
     else if (s1[1] == 'x') e1 = EDGE1X;
     else goto bad_edge;
     break;
    case 'x':
     if (s1[1] == '0') e1 = EDGEX0;
     else if (s1[1] == '1') e1 = EDGEX1;
     else goto bad_edge;
     break;
    default: goto bad_edge;
   }
   if ((etmp & e1) != 0)
    __pv_fwarn(577, "edge %s repeated in edge list", s1);
   else etmp |= e1;
   /* notice last edge will be vv with __toktyp of ] - must proces last */
   if (__toktyp == RSB) break;
  }
 __letendnum_state = FALSE;
 *edge = etmp;
 return(TRUE);
}

/*
 * read an notifier 
 * know leading , read and reads just the notifier reg
 */
static struct sy_t *rd_notifier(void) 
{ 
 struct sy_t *syp;

 __get_vtok();
 if (__toktyp != ID)
  {
bad_notfy:
   __pv_ferr(1285, "notifier register name expected - %s read",
    __prt_kywrd_vtok());
   return(NULL);
  } 
 /* this declares thing as a net - fixed later and checked even later */
 __last_xtk = -1;
 /* FIXME - since can never fail should change to arg terr */
 if (!__bld_expnode()) goto bad_notfy;
 __bld_xtree(0);
 syp = __root_ndp->lu.sy;
 /* type will be checked later */
 return(syp);
}

/*
 * ROUTINES TO PROCESS `language INCLUDE CONSTRUCT
 */

extern char __pv_ctab[];

/*
 * read lines after language up to `endlanguage
 *
 * know first token of line read and it is `language
 * reads through `endlanguage
 *
 * both `language and `endlanguage lines passed to call back if registered
 */
extern void __do_foreign_lang(void)
{
 register char *chp, *chp2;
 int first_time, done, savfnam_ind, sav_lin_cnt;

 if (!__rding_top_level || __rescanning_lib)
  {
   if (!__rescanning_lib)
    {
     __pv_ferr(2657,
      "`language compiler directive inside Verilog construct - skipping to `endlanguage");
    }
   savfnam_ind = __cur_fnam_ind;
   sav_lin_cnt = __lin_cnt;
   if (__notokenize_skiplines("`endlanguage") == TEOF)
    {
     __pv_terr(327,
      "skipped `language line %s no matching `endlanguage before **EOF**",
      __bld_lineloc(__xs, (unsigned) savfnam_ind, sav_lin_cnt));  
    }
   if (__langstr != NULL) strcpy(__langstr, "");
   return;
  }
 if (__lib_verbose || __verbose)
  {
   __cv_msg("  Processing `language directive at **%s(%d)\n", 
    __in_fils[__cur_fnam_ind], __lin_cnt);
  }
 __total_lang_dirs++;
 if (__iact_state)
  {
   __ia_err(1401,
    "`language compiler directive illegal in interactive commands");
   __skipover_line();
   if (__langstr != NULL) strcpy(__langstr, "");
   return; 
  }
 if (__langstr == NULL) __langstr = __my_malloc(IDLEN + 1);

 __doing_langdir = TRUE;
 for (first_time = TRUE, done = FALSE;;)
  {
rd_again:
   chp = __langstr;
   /* this does not move line count to next line - code here must */
   if (__my_getlin(__langstr) == EOF) 
    {
     /* first try to pop some sort of outer nested thing */
     if (__pop_vifstk()) goto rd_again;
     /* next try to replace just finished 0th element with new input file */
     if (__cur_infi + 1 > __last_inf) goto eof_error;
     __cur_infi++; 
     if (!__open_sfil()) goto eof_error;
     /* know first token of file flag now on */
     __file_just_op = FALSE;
     __first_num_eol = TRUE; 
     goto rd_again;

eof_error:
     __pv_ferr(2657,
      "while processing foreign `language section **EOF** read before `endlanguage");
     if (first_time)
      {
       if (__langstr != NULL) strcpy(__langstr, "");
       __doing_langdir = FALSE;
       return;
      }

     strcpy(__langstr, "`endlanguage");
     __langstr[12] = '\n';
     __langstr[13] = '\0';
     done = TRUE;
     goto try_callback;
    }
   if (first_time)
    {
     char s1[IDLEN];

     if (strlen(__langstr) + 11 >= IDLEN)
      {
       __pv_ferr(2679,
        "`language section line too long (%d) - truncated", IDLEN - 1);
      }
     strcpy(s1, __langstr);
     strcpy(__langstr, "`language");
     if (s1[0] != ' ' && s1[0] != '\t') strcat(__langstr, " ");
     strcat(__langstr, s1);
     first_time = FALSE;
    }
   else
    {
     if ((chp = __match_cdir(__langstr, "`endlanguage")) != NULL) 
      done = TRUE;
     else if ((chp = __match_cdir(__langstr, "`include")) != NULL) 
      {
       chp2 = &(chp[8]);  
       while (vis_white_(*chp2)) chp2++;
       strcpy(__macwrkstr, chp2);
       /* correct for right line because line getting moves to next line */
       __do_include();
       __lin_cnt++;
       __total_rd_lines++;
       /* no need to set beginning of lne because new file just opened */
       goto rd_again;
      } 
     else chp = __langstr;
    }

try_callback:
   /* execute call back if any registered - if none just returns */
   /* by passing chp, know for `langauge/`endlanguage no leading whie space */
   __exec_vpi_langlinecbs(chp, __in_fils[__cur_fnam_ind], __lin_cnt);
   __lin_cnt++;
   __total_rd_lines++;
   /* this set first token on line using 2 step flag needed for push back */
   __first_num_eol = TRUE;
   if (done)
    {
     strcpy(__langstr, "");
     __doing_langdir = FALSE;
     return;
    }
  }
}

/*
 * routine to skip over lines to keyword (usually `endlanguage)
 *
 * special routine that can not tokenize since probably non Verilog
 * returns last character read on success else EOF on error of EOF
 *
 * this must read and expand `include files because `endlanguage can be
 * in included file
 */
extern int __notokenize_skiplines(char *match_prefix)
{
 register char *chp;
 int c;
 
 if (__langstr == NULL) __langstr = __my_malloc(IDLEN + 1);
 for (;;)
  {
   if ((c = __my_getlin(__langstr)) == EOF)
    {
     /* first try to pop some sort of outer nested thing */
     /* this sets line number to right outer line and no line here */
     if (__pop_vifstk()) continue;

     /* next try to replace just finished 0th element with new input file */
     if (__cur_infi + 1 > __last_inf) goto eof_error;
     __cur_infi++; 
     if (!__open_sfil()) goto eof_error;
     /* know first token of file flag now on */
     __file_just_op = FALSE;
     /* this set first token on line using 2 step flag needed for push back */
     __first_num_eol = TRUE; 
     continue;

eof_error:
     __pv_ferr(2657,
      "while processing foreign `language section **EOF** read before `endlanguage");
     return(TEOF);
    }

   if ((chp = __match_cdir(__langstr, match_prefix)) != NULL)
    {
     __lin_cnt++;
     __total_rd_lines++;
     /* this set first token on line using 2 step flag needed for push back */
     __first_num_eol = TRUE;
     break;
    }
   /* becausing section ifdefed out, just ignore include */
   __lin_cnt++;
   __total_rd_lines++;
   /* this set first token on line using 2 step flag needed for push back */
   __first_num_eol = TRUE;
  }
 return(UNDEF);
}

/*
 * match a directive prefix (may be leading white space) 
 * returns char ptr to first character of matched if matched
 * else returns nil if no match
 */
extern char *__match_cdir(char *lp, char *match_prefix)
{
 register char *chp;
 int slen;

 /* possible lang str not yet allocated */ 
 if (lp == NULL) return(NULL);
 slen = strlen(match_prefix);
 for (chp = __langstr;; chp++) { if (!vis_nonnl_white_(*chp)) break; }
 if (strncmp(chp, match_prefix, slen) == 0) return(chp); 
 return(NULL);
}

/*
 * execute vpi_control vpiInsertSource operation
 *
 * know in `endlanguage line callback or will not get here 
 *
 * BEWARE - this works because no longjmp in source reading
 */
extern int __exec_rdinserted_src(char *buf)
{
 register int vi;
 int sav_ecnt, retv, sav_vin_top, sav_lincnt, sav_cur_fnamind, len;
 struct vinstk_t **sav_vinstk;

 /* save lin_cnt to restore after buffer parsed */ 
 sav_lincnt = __lin_cnt; 
 sav_cur_fnamind = __cur_fnam_ind;

 sav_ecnt = __pv_err_cnt;
 /* save the nested open file stack */
 sav_vinstk = (struct vinstk_t **)
  __my_malloc((__vin_top + 1)*sizeof(struct vinstk_t *));
 /* this moves the ptrs to be pointed to by same */
 for (vi = 0; vi <= __vin_top; vi++)
  {
   sav_vinstk[vi] = __vinstk[vi]; 
   __vinstk[vi] = NULL;
  }
 sav_vin_top = __vin_top;
 __vin_top = -1;

 /* push string on top (only one now on) of read stack */
 __push_vinfil();
 __visp = __vinstk[__vin_top];
 __visp->vichp = __visp->vichp_beg = buf;
 len = strlen(buf);
 __visp->vichplen = len;
 __in_s = NULL;

 for (;;)
  {
   __get_vtok();
   if (__toktyp == TEOF) break;
   __rding_top_level = FALSE;
   __rd_ver_mod();
   __rding_top_level = TRUE;
   if (__toktyp == TEOF) break;
  }
 /* restore the nested open file stack */
 /* first free any allocated vin stk records from includes */
 for (vi = 0; vi < MAXFILNEST; vi++)
  {
   if (__vinstk[vi] != NULL)
    {
     __my_free((char *) __vinstk[vi], sizeof(struct vinstk_t));
     __vinstk[vi] = NULL;
    }
  }
 /* next copy back and restore */
 for (vi = 0; vi <= sav_vin_top; vi++) __vinstk[vi] = sav_vinstk[vi];
 __lin_cnt = sav_lincnt;
 __cur_fnam_ind = sav_cur_fnamind;
 __vin_top = sav_vin_top;
 __visp = __vinstk[__vin_top];
 __in_s = __visp->vi_s;

 /* LOOKATME - why is this needed */
 __toktyp = UNDEF;
 __lasttoktyp = UNDEF;

 if (__pv_err_cnt > sav_ecnt) retv = FALSE; else retv = TRUE;
 return(retv);
}

/*
 * VERILOG 2000 ATTRIBUTE READING ROUTINES
 */

/*
 * read, parse and build attribute list from attribute string
 * builds list and returns header of list or nil on error
 *
 * new verilog 2000 feature
 * know string between (* and *) stored on entry in attr name field
 * trick is to push string onto file stack as if it is no arg macro 
 *
 * expression value converted to constant number here because
 * attributes need to be used by tools that do not know pound param vals
 * i.e. can be fed, post generate source
 */
extern struct attr_t *__rd_parse_attribute(struct attr_t *rd_attrp)
{
 register char *chp; 
 int sav_ecnt, sav_tot_lines, sav_fnam_ind, attllen;
 struct attr_t *attrp, *attr_hd, *last_attrp;
 char *attlin, attnam[IDLEN];

 attrp = attr_hd = last_attrp = NULL;
 attlin = rd_attrp->attrnam;
 /* SJM 07/30/01 - need to read chars and parse out of global */
 /* needed so can free work attrnam after rec built */ 
 if ((attllen = strlen(attlin)) >= __attrparsestrlen - 1)
  {
   __attrparsestr = __my_realloc((char *) __attrparsestr, __attrparsestrlen,
    attllen + 1);
   __attrparsestrlen = attllen + 1;
  }
 strcpy(__attrparsestr, attlin); 

 /* need to save total lines read since counted in attr when collected */
 /* parsing here counts lines because new lines not escaped */
 sav_tot_lines = __total_rd_lines;
 sav_fnam_ind = __cur_fnam_ind;
 sav_ecnt = __pv_err_cnt;

 /* if currently reading file, must preserve line count */
 if (__visp->vi_s != NULL) __visp->vilin_cnt = __lin_cnt;
 /* push string on top of read stack */
 __push_vinfil();
 __visp->vichp = __visp->vichp_beg = __attrparsestr;

 /* make sure not freeing line even if somehow hit eof - never should */
 __visp->vichplen = -1;
 __in_s = NULL;
 /* DBG remove --- */
 if (__debug_flg) __dbg_msg("parsing attribute string %s\n", attlin);
 /* --- */

 __cur_fnam_ind = rd_attrp->attr_fnind;
 __lin_cnt = rd_attrp->attrlin_cnt;

 __get_vtok();
 /* ; added to end of saved attribute string if not there */
 if (__toktyp == SEMI) 
  {
   __pv_ferr(3405,
    "attribute_instance illegal - at least one attr_spec required");
chk_eol: 
   for (chp = __visp->vichp; *chp != '\0'; chp++)
    {
     if (!vis_white_(*chp))
      {
       __pv_ferr(3407,
        "attribute_instance comma separator expected - semicolon read");
       /* on error always skip to end of string - need EOF next read */
       while (*chp != '\0') chp++;
       goto done;
      }
    }
   goto done;
  }
 for (;;)
  {
   if (__toktyp != ID)
    {
     __pv_ferr(3404, "attribute name expected - %s read", __prt_vtok());
err_skip_eol:
     /* on error always skip to end of string - need EOF next read */
     for (chp = __visp->vichp; *chp != '\0'; chp++) ;
     goto done;
    }
   strcpy(attnam, __token);
   __get_vtok();
   __root_ndp = NULL; 
   if (__toktyp == EQ)
    {
     __get_vtok();
     /* LOOKATME - should try to resync on errors */
     __last_xtk = -1;
     /* on success (T), this reads either , or ; */
     if (!__col_comsemi(-1)) goto err_skip_eol;
     __bld_xtree(0);
     if (__expr_has_glb(__root_ndp) || !__src_rd_chk_paramexpr(__root_ndp, 0))
      {
       __pv_ferr(3404,
        "attr_spec for attribute %s expression error - defined parameters and constants only", 
        attnam); 
       /* need to still add value of x to prevent further errors */
       __free2_xtree(__root_ndp);
       __root_ndp->szu.xclen = WBITS;
       /* default value is on 1 (non zero) */
       __set_numval(__root_ndp, 1, 0, WBITS);  
      }
     else
      {
       /* because of previous check, this can not fail */
       __eval_param_rhs_tonum(__root_ndp);
      }
    }
   else __root_ndp = NULL;

   /* allocate in link in attribute */
   attrp = (struct attr_t *) __my_malloc(sizeof(struct attr_t));
   attrp->attr_tok = rd_attrp->attr_tok;
   attrp->attrnam = __pv_stralloc(attnam); 
   /* must eval. after all param setting is done */
   attrp->attr_xp = __root_ndp; 
   /* LOOKATME - think should just use attr inst loc */
   attrp->attr_fnind = __cur_fnam_ind;
   attrp->attrlin_cnt = __lin_cnt;
   attrp->attrnxt = NULL;
   if (last_attrp == NULL) attr_hd = attrp; else last_attrp->attrnxt = attrp;
   last_attrp = attrp;

   if (__toktyp == SEMI) goto chk_eol;
   if (__toktyp != COMMA)
    {
     __pv_ferr(3406, "attr_spec separator or end \"*)\" expected - %s read",
      __prt_vtok());
     goto err_skip_eol;
    }
   __get_vtok();
   continue;
  }

done:
 /* caller must free attribute string when pased for all instances */

 /* restore total lines read */
 __total_rd_lines = sav_tot_lines;
 __cur_fnam_ind = sav_fnam_ind;
 /* SJM 07/30/01 - was using visp but that was not set or index */
 __cur_fnam = __in_fils[__cur_fnam_ind];
 /* small memory leak if syntax error */
 if (__pv_err_cnt > sav_ecnt) return(NULL);
 /* emit warnings if attr duplicated with different value - inform if same */
 if (attr_hd != NULL) attr_hd = chk_dup_attrs(attr_hd);
 return(attr_hd);
}

/*
 * check attribute list for duplicates
 * if duplicate remove - if different value warn if same value inform
 * 
 * LOOKATME - if lots of attributes need to sort and match
 */
static struct attr_t *chk_dup_attrs(struct attr_t *attr_hd)
{
 register struct attr_t *attrp1, *attrp2, *last_attrp1;
 struct attr_t *new_attrhd, *attrp3;
 char s1[RECLEN], s2[RECLEN]; 

 new_attrhd = attr_hd;
 last_attrp1 = NULL;
 for (attrp1 = attr_hd; attrp1 != NULL;)
  {
   for (attrp2 = attrp1->attrnxt; attrp2 != NULL; attrp2 = attrp2->attrnxt)
    {
     if (strcmp(attrp1->attrnam, attrp2->attrnam) == 0) 
      {
       /* know both numbers but still use xpr cmp */ 
       if (__cmp_xpr(attrp1->attr_xp, attrp2->attr_xp) == 0)
        {
         __gfinform(3001, attrp2->attr_fnind, attrp2->attrlin_cnt,
          "attribute %s duplicated with same value (first at %s) - first discared",
         attrp1->attrnam, __bld_lineloc(s1, attrp1->attr_fnind,
         attrp1->attrlin_cnt));  
        }
       else
        {
         __gfwarn(3101, attrp2->attr_fnind, attrp2->attrlin_cnt,
          "attribute %s value %s duplicated with different values - first at %s value %s discarded",
         attrp1->attrnam, __msgexpr_tostr(s1, attrp2->attr_xp),
         __bld_lineloc(__xs, attrp1->attr_fnind, attrp1->attrlin_cnt), 
         __msgexpr_tostr(s2, attrp1->attr_xp));
        }
       /* SJM 10/16/00 - must set next before freeing and splicing */
       attrp3 = attrp1->attrnxt;

       /* splice out first - if more duplicates will catch later */
       if (last_attrp1 == NULL) new_attrhd = attrp1->attrnxt; 
       else last_attrp1->attrnxt = attrp1->attrnxt;
       __free_xtree(attrp1->attr_xp);

       __my_free((char *) attrp1, sizeof(struct attr_t));
       /* must not advance last attr */
       attrp1 = attrp3;
       goto chk_nxt_attr;
      }
    }
   attrp1 = attrp1->attrnxt;
   last_attrp1 = attrp1;     
chk_nxt_attr:;
  }
 return(new_attrhd);
}

