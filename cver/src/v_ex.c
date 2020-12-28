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
 * run time execution routines - statements but not expression eval
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <setjmp.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>

#include <signal.h>

#ifdef __DBMALLOC__
#include "../malloc.h"
#endif

#include "v.h"
#include "cvmacros.h"

/* local prototypes */
static void tr_resume_msg(void);
static void exec_stmts(register struct st_t *);
static void thrd_done_cleanup(register struct thread_t *);
static void brktr_exec_stmts(register struct st_t *);
static void step_exec_stmt(register struct st_t *);
static int stepped_to_new_loc(struct st_t *);
static void eval_tskassign_rhsexpr(register struct xstk_t *, register int,
 register int, register int, register int);
static void tr_proc_assign(struct st_t *, struct xstk_t *);
static void tr_nbproc_assign(struct st_t *, struct xstk_t *);
static struct st_t *exec_rep_ectl_setup(struct st_t *stp);
static int exec_dctrl(struct st_t *);
static void sched_proc_delay(struct delctrl_t *, word *, int);
static void sched_nbproc_delay(struct delctrl_t *, struct xstk_t *,
 struct st_t *);
static void arm_event_dctrl(struct delctrl_t *, register word *, int);
static void arm_nbevent_dctrl(struct delctrl_t *, struct xstk_t *,
 struct st_t *);
static struct st_t *exec_case(struct st_t *);
static struct st_t *exec_real_case(struct st_t *);
static void tr_case_st(struct xstk_t *, int);
static struct st_t *exec_casex(struct st_t *);
static struct st_t *exec_casez(struct st_t *);
static int exec_wait(register struct st_t *);
static int for_not_done(struct for_t *);
static void exec_namblk(struct st_t *);
static struct thread_t *sched_fj_subthread(struct st_t *stp);
static void init_thrd(register struct thread_t *);
static void tradd_tf_argval(int, struct net_t *, struct xstk_t *);
static void store_tskcall_outs(struct st_t *);
static void grow_fcstk(void);
static void exec_count_drivers(struct expr_t *);
static void exec_testplusargs(struct expr_t *);
static void exec_scanplusargs(struct expr_t *);
static void exec_1arg_transcendental(int, struct expr_t *);
static void exec_transcendental_int(struct expr_t *);
static void exec_transcendental_sign(struct expr_t *);
static void exec_transcendental_powsign(int, struct expr_t *);
static void exec_transcendental_minmax(int, struct expr_t *);
static void exec_transcendental_atan2(struct expr_t *);
static void exec_transcendental_hypot(struct expr_t *);
static void exec_cause(struct st_t *);
static struct thread_t *find_hgh_sametskthrd(struct thread_t *);
static int thread_above_cur(struct thread_t *);
static void free_thd_stuff(struct thread_t *);
static void unlink_tskthd(struct thread_t *);
static int chk_strobe_infloop(struct st_t *, struct sy_t *);
static void mcd_do_fclose(struct expr_t *);
static unsigned bld_open_mcd(void);
static int unsigned mc_do_fopen(struct expr_t *);
static void do_showvars_stask(struct expr_t *);
static void do_warn_supp_chg(char *, struct expr_t *, int);
static void do_reset(struct expr_t *);
static void do_showscopes(struct expr_t *);
static void prt_1m_scopelist(struct itree_t *);
static void prt_1tsk_scopelist(struct task_t *, int);
static void prt_1m_nestscopes(struct itree_t *);
static void prt_1tsk_nestscopes(struct symtab_t *);
static void exec_qfull(struct expr_t *);
static void do_q_init(struct expr_t *);
static struct q_hdr_t *find_q_from_id(int);
static void init_q(struct q_hdr_t *);
static void do_q_add(struct expr_t *);
static void do_q_remove(struct expr_t *);
static void do_q_examine(struct expr_t *);
static void cmp_mean_interarriv_tim(word64 *, struct q_hdr_t *);
static void cmp_max_wait(word64 *, struct q_hdr_t *);
static void cmp_mean_wait_tim(word64 *, struct q_hdr_t *);
static void exec_prttimscale(struct expr_t *);
static void exec_timefmt(struct expr_t *);
static int get_opt_starg(struct expr_t *, int);
static void exec_log_fnamchg(struct expr_t *);
static void exec_trace_fnamchg(struct expr_t *);
static void exec_expr_schg(struct expr_t *);
static void free_thd_subtree(struct thread_t *);
static void suspend_curthd(struct st_t *);


/* extern prototypes (maybe defined in this module) */
extern int __comp_sigint_handler(void);
extern char *__my_malloc(int);
extern char *__pv_stralloc(char *);
extern char *__my_realloc(char *, int, int);
extern struct xstk_t *__eval_assign_rhsexpr(register struct expr_t *,
 register struct expr_t *);
extern struct thread_t *__setup_tsk_thread(struct task_t *);
extern void __sched_fork(struct st_t *);
extern i_tev_ndx __bld_nb_tev(struct st_t *, struct xstk_t *, word64);
extern int __lhsexpr_var_ndx(register struct expr_t *);
extern void __eval_lhsexpr_var_ndxes(register struct expr_t *);
extern struct st_t *__brktr_exec_1stmt(struct st_t *);
extern struct thread_t *__alloc_thrd(void);
extern struct st_t *__exec_tskcall(struct st_t *);
extern struct xstk_t *__eval2_xpr(struct expr_t *);
extern struct expr_t *__sim_copy_expr(struct expr_t *);
extern int __comp_ndx(register struct net_t *, register struct expr_t *);
extern struct expr_t *__bld_rng_numxpr(word, word, int);
extern void __free_xtree(struct expr_t *);
extern char *__regab_tostr(char *, word *, word *, int, int, int);
extern char *__xregab_tostr(char *, word *, word *, int, struct expr_t *);
extern char *__msgexpr_tostr(char *, struct expr_t *);
extern char *__to_idnam(struct expr_t *);
extern char *__msg_blditree(char *, struct itree_t *, struct task_t *);
extern char *__msg2_blditree(char *, struct itree_t *);
extern char *__bld_lineloc(char *, unsigned, int);
extern char *__to_timunitnam(char *, unsigned);
extern char *__to_timstr(char *, word64 *);
extern char *__to_tetyp(char *, unsigned); 
extern char *__to_tsktyp(char *, unsigned);
extern double __unscale_realticks(word64 *, struct mod_t *);
extern FILE *__tilde_fopen(char *, char *);
extern struct xstk_t *__ld_wire_driver(register struct net_pin_t *);
extern struct xstk_t *__ld_stwire_driver(register struct net_pin_t *);
extern int __has_vpi_driver(struct net_t *, struct net_pin_t *);
extern char *__schop(char *, char *);
extern char *__to_dcenam(char *, unsigned);
extern struct xstk_t *__cstr_to_vval(char *);
extern struct task_t *__getcur_scope_tsk(void);
extern unsigned __mc1_fopen(char *, int, int);
extern unsigned __close_mcd(unsigned, int);
extern void __wrap_puts(char *, FILE *);
extern void __wrap_putc(int, FILE *);
extern void __evtr_resume_msg(void);
extern void __do_iact_disable(struct hctrl_t *, int);
extern void __dmp_thrd_info(struct thread_t *);
extern void __dmp_tskthd(struct task_t *, struct mod_t *);
extern void __my_free(char *, int);
extern void __exec2_proc_assign(struct expr_t *, register word *,
 register word *);
extern int __cvt_lngbool(word *, word *, int);
extern int __wide_vval_is0(register word *, int);
extern void __exec_qc_assign(struct st_t *, int);
extern void __exec_qc_wireforce(struct st_t *);
extern void __exec_qc_deassign(struct st_t *, int);
extern void __exec_qc_wirerelease(struct st_t *);
extern int __process_brkpt(struct st_t *);
extern void __prt_src_lines(int, int, int);
extern void __cnv_stk_fromreg_toreal(struct xstk_t *, int);
extern void __cnv_stk_fromreal_toreg32(struct xstk_t *);
extern void __sizchgxs(struct xstk_t *, int);  
extern void __dmp_proc_assgn(FILE *, struct st_t *, struct delctrl_t *, int);
extern void __trunc_exprline(int, int);
extern void __dmp_nbproc_assgn(FILE *, struct st_t *, struct delctrl_t *);
extern void __dmp_dctrl(FILE *, struct delctrl_t *);
extern void __chg_xprline_size(int);
extern void __dmp_dcxpr(FILE *, union del_u, unsigned);
extern void __get_del(register word64 *, register union del_u, unsigned);
extern void __insert_event(register i_tev_ndx);
extern void __dmp_forhdr(FILE *, struct for_t *);
extern void __add_ev_to_front(register i_tev_ndx);
extern void __dmp_tskcall(FILE *, struct st_t *);
extern void __xmrpush_refgrp_to_targ(struct gref_t *);
extern void __adds(char *);
extern void __chg_st_val(struct net_t *, register word *, register word *);
extern void __grow_xstk(void);
extern void __chg_xstk_width(struct xstk_t *, int);
extern void __grow_tevtab(void);
extern void __ld_wire_val(register word *, register word *, struct net_t *);
extern void __do_interactive_loop(void);
extern void __cnv_ticks_tonum64(word64 *, word64, struct mod_t *);
extern void __exec_sfrand(struct expr_t *);
extern void __exec_scale(struct expr_t *);
extern void __pli_func_calltf(struct expr_t *);
extern void __vpi_sysf_calltf(struct expr_t *);
extern void __get_bidnpp_sect(struct net_t *, struct net_pin_t *, int *,
 int *);
extern char *__get_eval_cstr(struct expr_t *, int *);
extern void __free_1thd(struct thread_t *);
extern struct st_t *__exec_stsk(struct st_t *, struct sy_t *,
 struct tskcall_t *);
extern void __free_thd_list(struct thread_t *);
extern int __exec_disable(struct expr_t *);
extern void __do_disp(register struct expr_t *, int);
extern void __fio_do_disp(register struct expr_t *, int, int, char *); 
extern void __start_fmonitor(struct st_t *);
extern void __dmpmod_nplst(struct mod_t *, int);
extern void __start_monitor(struct st_t *);
extern void __exec_readmem(struct expr_t *, int);
extern void __exec_sreadmem(struct expr_t *, int);
extern void __exec_dumpvars(struct expr_t *);
extern int __get_eval_word(struct expr_t *, word *);
extern void __exec_input_fnamchg(struct expr_t *);
extern void __exec_history_list(int);
extern void __do_scope_list(void);
extern void __exec_sdf_annotate_systsk(struct expr_t *);
extern void __call_misctfs_finish(void);
extern void __vpi_endsim_trycall(void);
extern void __emit_stsk_endmsg(void);
extern void __maybe_open_trfile(void);
extern void __escape_to_shell(char *);
extern void __write_snapshot(int);
extern void __prt2_mod_typetab(int);
extern void __pli_task_calltf(struct st_t *);
extern void __vpi_syst_calltf(struct st_t *);
extern void __my_fclose(FILE *);
extern void __emit_1showvar(struct net_t *, struct gref_t *);
extern void __prt_top_mods(void);
extern void __disp_itree_path(struct itree_t *, struct task_t *);
extern void __set_scopchg_listline(void);
extern void __call_misctfs_scope(void);
extern void __vpi_iactscopechg_trycall(void);
extern void __my_ftime(time_t *, time_t *);
extern void __prt_end_msg(void);
extern void __exec_dist_uniform(struct expr_t *);
extern void __exec_dist_stdnorm(struct expr_t *);
extern void __exec_dist_exp(struct expr_t *);
extern void __exec_dist_poisson(struct expr_t *);
extern void __exec_chi_square(struct expr_t *);
extern void __exec_dist_t(struct expr_t *);
/* ??? extern void __dmp_event_tab(void); */
extern void __my_dv_flush(void);
extern void __add_nchglst_el(register struct net_t *);
extern void __add_dmpv_chglst_el(struct net_t *);
extern void __wakeup_delay_ctrls(register struct net_t *, register int,
 register int);
extern void __dmp_all_thrds(void);
extern double __cnvt_stk_to_real(struct xstk_t *, int);
extern int __enum_is_suppressable(int);
extern char *__to_sttyp(char *, unsigned);
extern int __trim1_0val(word *, int);
extern char *__vval_to_vstr(word *, int, int *);
extern void __vstr_to_vval(word *, char *, int);
extern int __is_vdigit(int, int);
extern void __to_dhboval(int, int);
extern double __my_strtod(char *, char **, int *);



extern void __tr_msg(char *, ...);
extern void __cv_msg(char *, ...);
extern void __cvsim_msg(char *, ...);
extern void __sgfwarn(int, char *, ...);
extern void __sgferr(int, char *, ...);
extern void __dbg_msg(char *, ...);
extern void __sgfinform(int, char *, ...);
extern void __arg_terr(char *, int);
extern void __case_terr(char *, int);
extern void __misc_terr(char *, int);
extern void __misc_sgfterr(char *, int);
extern void __my_exit(int, int);
extern void __my_fprintf(FILE *, char *, ...);

/* reset mechanism long jump buffer */
extern jmp_buf __reset_jmpbuf;

/* system stuff */
extern int errno;

/* some general evaluation tables */
word __masktab[] = {
 /* since 0 is the same as all used, mask must be entire word */
 0xffffffffL, 0x00000001L, 0x00000003L, 0x00000007L,
 0x0000000fL, 0x0000001fL, 0x0000003fL, 0x0000007fL,
 0x000000ffL, 0x000001ffL, 0x000003ffL, 0x000007ffL,
 0x00000fffL, 0x00001fffL, 0x00003fffL, 0x00007fffL,
 0x0000ffffL, 0x0001ffffL, 0x0003ffffL, 0x0007ffffL,
 0x000fffffL, 0x001fffffL, 0x003fffffL, 0x007fffffL,
 0x00ffffffL, 0x01ffffffL, 0x03ffffffL, 0x07ffffffL,
 0x0fffffffL, 0x1fffffffL, 0x3fffffffL, 0x7fffffffL,
 /* special for places where mask uses length i.e. 32 bits */
 0xffffffffL
};

extern double __dbl_toticks_tab[];


/*
 * ROUTINES TO PROCESS PROCEDURAL EVENTS AND EXECUTE BEHAVIORAL STATEMENTS
 */

/*
 * execute a control thread from one event suspension until next
 * need to handle rhs delay control and => proc. assignment
 *
 * when thread completes just removes and continues with other threads
 * know if this suspends or hits ctrl-c will always build and schedule new ev
 * possible for thread next statement to be nil to terminate thread 
 * and here must be left and terminated after suspend
 */
extern void __process_thrd_ev(register struct tev_t *tevp)
{
 register struct st_t *stp;
 struct st_t *stp2;
 struct thread_t *parthp;

 __proc_thrd_tevents++;
 __suspended_thd = NULL;
 __suspended_itp = NULL;
 /* set current thread and remove connection of thread to event */
 __cur_thd = tevp->tu.tethrd;
 __cur_thd->thdtevi = -1;

 /* if not func. must have change itree to right one for thread */
 /* NO - this will not be be true if invoked xmr task - inst ptr. diff */  
 /* but will be put back when xmr task done so ok */ 
 /* DBG remove -- 
 if (__fcspi == -1 && __cur_thd->th_itp != __inst_ptr)
  __misc_terr(__FILE__, __LINE__);
 --- */
 stp = __cur_thd->thnxtstp;

 /* possible to remove thread even though no more statements to exec */
 if (stp != NULL && (__st_tracing || __ev_tracing))
  {
   __slin_cnt = stp->stlin_cnt;
   __sfnam_ind = stp->stfnam_ind;

   if (__st_tracing) tr_resume_msg(); else __evtr_resume_msg();
   __tr_msg("-- resuming at statement %s\n",
    __bld_lineloc(__xs, stp->stfnam_ind, stp->stlin_cnt));
  }

 /* for each completed thread continue in parent without schd */
 /* loop because continues until thread tree done or suspend */
 for (__stmt_suspend = FALSE;;)
  {
   /* keep executing behavioral stmts until done or hit timing control */
   if (stp != NULL) 
    {
     /* even if single stepping must not see iact threads */
     /* since this always either hits end of thread or suspends */
     if (__single_step && __cur_thd->th_hctrl == NULL)
      step_exec_stmt(stp);
     /* but batch tracing traces */
     else if (__st_tracing)
      {
        brktr_exec_stmts(stp);
      }
     else exec_stmts(stp);

     /* on suspend event itree location is right for exec */
     /* if no suspend but current ctrl thread (init/always/task) got to end */ 
     /* fall thru and try to immediately exec parent */
     if (__stmt_suspend) break;
    }

   /* DBG remove --- */
   if (__cur_thd->thdtevi != -1) __misc_terr(__FILE__, __LINE__);
   /* --- */
   /* this thread tree done if nil - can only be interactive or init/always */
   /* this handles all freeing because entire thread tree done */
   if ((parthp = __cur_thd->thpar) == NULL)
    {
     /* if interactive thread - free and set possible history disabled */
     if (__cur_thd->th_hctrl != NULL)
      __do_iact_disable(__cur_thd->th_hctrl, FALSE);
     __stmt_suspend = TRUE;
     break;
    }

   /* know if task has outs will always have parent */
   /* store parameters if needed */
   if (__cur_thd->tsk_stouts)
    { 
     /* DBG remove --- */ 
     if (!parthp->th_postamble) __misc_terr(__FILE__, __LINE__);
     /* --- */
     /* if disabled do not store parameters, but still adjust nxt stp */
     /* not parent must be set to continue at tsk call for storing outs */
     if (!__cur_thd->th_dsable) store_tskcall_outs(parthp->thnxtstp);
 
     /* SJM 08/18/02 - must fixup including skip of non loop end gotos */
     /* now that store of tsk outs finished */
     stp2 = parthp->thnxtstp;
     if (stp2 != NULL) stp2 = stp2->stnxt;
     if (stp2 == NULL) parthp->thnxtstp = NULL;
     else if (stp2->stmttyp != S_GOTO) parthp->thnxtstp = stp2;
     else if (stp2->lpend_goto) parthp->thnxtstp = stp2;
     else
      {
       for (;;)
        {
         /* know here stp2 is non loop end goto - moves to goto first */
         if ((stp2 = stp2->st.sgoto) == NULL || stp2->stmttyp != S_GOTO)
          { parthp->thnxtstp = stp2; break; }
         if (stp2->lpend_goto) { parthp->thnxtstp = stp2; break; }
        }
      }
     /* ??? REPLACED parthp->thnxtstp = parthp->thnxtstp->stnxt; */
     parthp->th_postamble = FALSE;
    }   

   /* DBG remove --- */
   if (__cur_thd->th_ialw) __misc_terr(__FILE__, __LINE__); 
   if (parthp->thofscnt == 0) __misc_terr(__FILE__, __LINE__);
   if (__debug_flg)
    { __dbg_msg("*** thread finished:\n"); __dmp_thrd_info(__cur_thd); }
   /* --- */

   /* this thread finished - remove it from control thread d.s. */
   thrd_done_cleanup(parthp);

   /* more fork-join subthreads to complete */
   if (parthp->thofscnt > 0) { __stmt_suspend = TRUE; break; }

   /* all subthreads finished, continue with parent */
   /* for enabled task (not named block), know out arg. store phase done */
   parthp->thofs = NULL;
   /* continue with parent by executing next statement */
   /* no suspend here */
   __cur_thd = parthp;
   __pop_itstk();
   __push_itstk(__cur_thd->th_itp);
   stp = __cur_thd->thnxtstp;
  }
 /* DBG remove
 if (!__stmt_suspend) __misc_terr(__FILE__, __LINE__);
 --- */
 /* only have current thread when evaling thread event */
 __cur_thd = NULL;
}

/*
 * routine to clean up linked thread control structure after thread done
 *
 * thread finished - clean up and try to continue in parent
 * this removes various connected stuff but leave thread fields
 *
 * when done no current thread caller must set if needed
 */
static void thrd_done_cleanup(register struct thread_t *parthp)
{
 free_thd_stuff(__cur_thd);

 /* move up and continue in parent */
 parthp->thofscnt -= 1;
 /* one thread of fork/join done - link it out after redundant cnt dec */
 if (__cur_thd->thleft != NULL)
  __cur_thd->thleft->thright = __cur_thd->thright;
 /* adjust parent's thread ofset if removing first in list */
 else parthp->thofs = __cur_thd->thright;

 if (__cur_thd->thright != NULL)
  __cur_thd->thright->thleft = __cur_thd->thleft;
 /* free stuff already removed and events canceled so just free */
 __my_free((char *) __cur_thd, sizeof(struct thread_t));
 __cur_thd = NULL; 

 /* RELEASE remove ---
 if (parthp->thofscnt == 1)
  {
   if (parthp->thofs->thright != NULL
    || parthp->thofs->thleft != NULL) __misc_terr(__FILE__, __LINE__);
  }
 --- */
}

/*
 * print out trace location and time states
 *
 * no leading new line may need to have separate trace file if user output
 * leaves unfinished lines.
 *
 * for statement tracing only change file name when module changes
 * so line number will be in same * file
 */
static void tr_resume_msg(void)
{
 char s1[RECLEN], s2[RECLEN];

 if (__inst_ptr != __last_tritp)
  {
   __tr_msg("==> tracing in %s (%s) line %s\n",
    __msg2_blditree(s1, __inst_ptr), __inst_ptr->itip->imsym->synam,
    __bld_lineloc(s2, (unsigned) __sfnam_ind, __slin_cnt));
   __last_tritp = __inst_ptr;
  }
 if (__last_trtime != __simtime)
  {
   /* this should go through time format ? */
   __tr_msg("<<< tracing at time %s\n", __to_timstr(s1, &__simtime));
   __last_trtime = __simtime;
  }
}

/*
 * execute statement list
 * called from thrd event processing routine and return when blocked or done
 * execute until fall off end (thread done) or schedule wake up event
 */
static void exec_stmts(register struct st_t *stp)
{
 register word val;
 register struct xstk_t *xsp;
 int tmp, wlen;
 struct st_t *stp2;
 struct for_t *forp;
 struct expr_t *cntx;

 /* notice one pass through loop executes exactly 1 statement */
 for (;;)
  {
   __slin_cnt = stp->stlin_cnt;
   __sfnam_ind = stp->stfnam_ind;
   __num_execstmts++;
   /* DBG remove --
   if (__cur_thd == NULL || __cur_thd->th_itp != __inst_ptr)
    __misc_terr(__FILE__, __LINE__);
   --- */

   switch ((byte) stp->stmttyp) {
    /* SJM - 02/08/02 - should not count empties as exec stmts */
    case S_NULL: case S_STNONE: __num_execstmts--; break;
    case S_FORASSGN:
     __num_addedexec++; 
     /* FALLTHRU */
    case S_PROCA:
     xsp = __eval_assign_rhsexpr(stp->st.spra.rhsx, stp->st.spra.lhsx);
     __exec2_proc_assign(stp->st.spra.lhsx, xsp->ap, xsp->bp);
     __pop_xstk();
     break;
    case S_NBPROCA:
     /* only non delay form non blocking assign exec here - implied #0 */
     xsp = __eval_assign_rhsexpr(stp->st.spra.rhsx, stp->st.spra.lhsx);
     sched_nbproc_delay((struct delctrl_t *) NULL, xsp, stp);
     __pop_xstk();
     break;
    case S_RHSDEPROCA:
     /* notice this statement never executed directly - delctrl execed */ 
     /* then after block - results execed here */
     wlen = wlen_(stp->st.spra.lhsx->szu.xclen);
     /* know rhs width here same as lhs width */
     __exec2_proc_assign(stp->st.spra.lhsx, __cur_thd->th_rhswp,
      &(__cur_thd->th_rhswp[wlen]));
     /* must reset and free pending saved rhs over schedule */
     __my_free((char *) __cur_thd->th_rhswp, 2*wlen*WRDBYTES);
     __cur_thd->th_rhswp = NULL;
     __cur_thd->th_rhswlen = -1;
     __cur_thd->th_rhsform = FALSE;
     break;
    case S_IF:
     xsp = __eval_xpr(stp->st.sif.condx);
     /* condition T (non zero) only if at least 1, 1 */
     if (xsp->xslen <= WBITS)
      {
       /* SJM 07/20/00 - must convert to real if real */
       if (stp->st.sif.condx->is_real)
        {
         double d1;
 
         memcpy(&d1, xsp->ap, sizeof(double));
         tmp = (d1 != 0.0);
        }
       else tmp = ((xsp->ap[0] & ~xsp->bp[0]) != 0L);
      }
     else tmp = (__cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen)) == 1);
     __pop_xstk();
     if (tmp) stp = stp->st.sif.thenst;
     else if (stp->st.sif.elsest != NULL) stp = stp->st.sif.elsest;
     else stp = stp->stnxt;
     goto nxt_stmt;
    case S_CASE:
     /* notice Verilog cases cannot fall thru */
     if ((stp2 = exec_case(stp)) == NULL) break;
     stp = stp2;
     goto nxt_stmt;
    case S_FOREVER: stp = stp->st.swh.lpst; goto nxt_stmt;
    case S_REPSETUP:
     /* know repeat stmt follows rep setup */
     __num_addedexec++; 
     cntx = stp->stnxt->st.srpt.repx;
     xsp = __eval_xpr(cntx);
     /* SJM 04/02/02 - real count must be converted to word/int */
     if (cntx->is_real) __cnv_stk_fromreal_toreg32(xsp);
     if (xsp->xslen > WBITS) __sizchgxs(xsp, WBITS);
     if (xsp->ap[1] != 0L)
      {
       __sgfwarn(645,
        "repeat loop in %s count has x/z expression value - loop skipped",
        __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
       val = 0;
      }
     else
      {
       /* SJM 04/02/02 - if repeat count signed and negative, never exec */ 
       if (cntx->has_sign && ((int) xsp->ap[0]) <= 0) val = 0;
       else val = xsp->ap[0];
      }
     __pop_xstk();
     /* notice count must be converted to unsigned with neg set to 0 */
     /* set to 0 so after inced here and initial repeat exec dec, */
     stp->stnxt->st.srpt.reptemp[__inum] = ++val;
     break;
    case S_REPEAT:
     if ((val = --(stp->st.srpt.reptemp[__inum])) == 0L) break; 
     stp = stp->st.srpt.repst;
     goto nxt_stmt;
    case S_WHILE:
     xsp = __eval_xpr(stp->st.swh.lpx);
     if (xsp->xslen <= WBITS)
      {
       /* SJM 07/20/00 - must convert to real if real */
       if (stp->st.swh.lpx->is_real)
        {
         double d1;
  
         memcpy(&d1, xsp->ap, sizeof(double));
         __pop_xstk();
         /* must not emit informs from val if real */
         if (d1 != 0.0) { stp = stp->st.swh.lpst; goto nxt_stmt; }
         break;
        }
       val = xsp->bp[0];
       if ((xsp->ap[0] & ~val) != 0L)
        {
         if (val != 0) 
          {
           __sgfinform(403, "while in %s condition true but some bits x/z",
            __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
          }
         __pop_xstk();
         stp = stp->st.swh.lpst;
         goto nxt_stmt;
        }
       /* notice any 1 implies true so will not get here */
       if (val != 0)
        {
         __sgfinform(402,
          "while loop in %s terminating false condition value has x/z bits",
          __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
        }
       __pop_xstk();
       break;
      } 
     if ((tmp = __cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen))) == 1)
      {
       if (!vval_is0_(xsp->bp, xsp->xslen))
        {
         __sgfinform(403, "while condition in %s true but some bits x/z",
          __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
        }
       __pop_xstk();
       stp = stp->st.swh.lpst;
       goto nxt_stmt;
      }
     __pop_xstk();
     /* notice any 1 implies true so will not get here */
     if (tmp == 3)
      {
       __sgfinform(402,
        "while loop terminating false condition in %s value has x/z bits",
        __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
     }
     break;
    case S_WAIT:
     /* on true expression, returns true */
     if (exec_wait(stp)) { stp = stp->st.swait.lpst; goto nxt_stmt; }
     /* is this unnecessary since action stmt points back to wait */
     __cur_thd->thnxtstp = stp;
     __stmt_suspend = TRUE;
     return;
    case S_FOR:
     /* when loop done, for returns NULL as next stmt else 1st body st. */
     forp = stp->st.sfor;
     /* F when done */
     if (!for_not_done(forp))
      {
       break;
      }
     stp = forp->forbody;
     goto nxt_stmt;
    case S_REPDCSETUP:
     stp = exec_rep_ectl_setup(stp);
     goto nxt_stmt;
    case S_DELCTRL:
     /* this returns F, for suspend, non blocking returns T */
     /* 10/27/00 SJM - for repeat rhs ectrl count x/z <= 0 assign */
     /* immediate so also return T */
     if (exec_dctrl(stp)) { stp = __cur_thd->thnxtstp; goto nxt_stmt; }
     __stmt_suspend = TRUE;
     return;
    case S_NAMBLK:
     /* for function only, just continue in named block */
     if (__fcspi >= 0) { stp = stp->st.snbtsk->tskst; goto nxt_stmt; }
     exec_namblk(stp);
     stp = __cur_thd->thnxtstp;
     goto nxt_stmt;
    case S_UNBLK:
     stp = stp->st.sbsts;
     goto nxt_stmt;
    case S_UNFJ:
     /* this is unnamed fork-join only */ 
     __sched_fork(stp);
     __cur_thd->thnxtstp = stp->stnxt;
     __stmt_suspend = TRUE;
     return;
    case S_TSKCALL:
     /* if system task, NULL will suspend, else continue in down thread */
     if ((stp2 = __exec_tskcall(stp)) == NULL) return;
     stp = stp2;
     goto nxt_stmt;
    case S_QCONTA:
     if (stp->st.sqca->qcatyp == ASSIGN) __exec_qc_assign(stp, FALSE);
     else
      {
       /* force of reg, is like assign except overrides assign */
       if (stp->st.sqca->regform) __exec_qc_assign(stp, TRUE);
       else __exec_qc_wireforce(stp);
      }
     break;
    case S_QCONTDEA:
     if (stp->st.sqcdea.qcdatyp == DEASSIGN) __exec_qc_deassign(stp, FALSE);
     else
      {
       if (stp->st.sqcdea.regform) __exec_qc_deassign(stp, TRUE);
       else __exec_qc_wirerelease(stp);
      } 
     break;
    case S_CAUSE:
     exec_cause(stp);
     break;
    case S_DSABLE:
     /* if function, disable means continue with statement after block */ 
     /* if disable of func. next statement is nil, so done with func. */
     if (__fcspi >= 0) { stp = stp->st.sdsable.func_nxtstp; goto nxt_stmt; }

     if (__exec_disable(stp->st.sdsable.dsablx)) goto thread_done;
     /* disable elsewhere in control tree means just continue here */
     break;
    case S_GOTO:
     stp = stp->st.sgoto;
     /* notice goto of nil, ok just means done */ 
     __num_addedexec++; 
     goto nxt_stmt;
    case S_BRKPT:
     /* returns T on need to break */
     if (__process_brkpt(stp)) goto nxt_stmt;

     /* not a break for some reason - restore stmt type and exec 1 stmt */
     /* if bp halt off 2nd time through after break, this execs */
     stp->stmttyp = stp->rl_stmttyp;
     /* execute the broken on stmt */
     stp2 = __brktr_exec_1stmt(stp);
     /* put back break pt. and make returned next stp as stp */
     stp->stmttyp = S_BRKPT;
     /* if nil will check to see if suspend or end of thread */
     stp = stp2;
     goto nxt_stmt;
    default: __case_terr(__FILE__, __LINE__);
   }
   stp = stp->stnxt;
nxt_stmt:
   if (stp == NULL) break;
   /* entry from exec of interactive command only if ctrl c hit */
   if (__pending_enter_iact)  
    { __stmt_suspend = TRUE; suspend_curthd(stp); return; }
  }
 /* when done with current function just return */
 if (__stmt_suspend || __fcspi >= 0) return;
thread_done:
 __stmt_suspend = FALSE;
 __cur_thd->thnxtstp = NULL;
}

/*
 * tracing and break point processing version of exec statements
 * called from thrd event processing routine and return when blocked or done
 * execute until fall off end (thread done) or schedule wake up event
 */
static void brktr_exec_stmts(register struct st_t *stp)
{
 /* notice one pass through loop executes exactly 1 statement */
 for (;;)
  {
   /* here if nil returned force suspend - exec set thread next statement */
   stp = __brktr_exec_1stmt(stp);
   if (stp == NULL) break;
   /* if done with thread, will detect enter iact flag in higher routine */ 
   if (__pending_enter_iact)  
    { __stmt_suspend = TRUE; suspend_curthd(stp); return; }
  }
 if (__stmt_suspend) return;
 __cur_thd->thnxtstp = NULL;
}

/*
 * exec statements while stepping
 * special case if break hit in here
 * called from thrd event processing routine and return when blocked or done
 * execute until fall off end (thread done) or schedule wake up event
 */
static void step_exec_stmt(register struct st_t *stp)
{
 /* notice one pass through loop executes exactly 1 statement */
 for (;;)
  {
   /* if step command when iact entered from iact thread or ^c step to 1st */
   /* of new thread not one more statement */
   if (__step_from_thread) stp = __brktr_exec_1stmt(stp);
   else __step_from_thread = TRUE;

   /* if hit break point while stepping, disable stepping and return */
   /* suspend already done */
   if (__pending_enter_iact && __iact_reason == IAER_BRKPT)
    {
     __single_step = FALSE;
     __step_rep_cnt = 0;  
     __step_match_itp = NULL; 
     __verbose_step = FALSE;
     /* since rexec stmt, must have current thread */
     /* DBG remove --- */
     if (stp == NULL || __cur_thd == NULL) __misc_terr(__FILE__, __LINE__);
     /* --- */
     __last_stepitp = __cur_thd->th_itp;
     __last_steptskp = __cur_thd->assoc_tsk;
     __last_stepifi = (int) stp->stfnam_ind;
     __step_lini = stp->stlin_cnt;
     __step_ifi = (int) stp->stfnam_ind;
     /* must suspend */
     __stmt_suspend = TRUE;
     suspend_curthd(stp);
     return;
    }
   /* must exit loop since done with this thread */
   if (stp == NULL)
    {
     if (!__stmt_suspend)
      { __step_lini = -1; __step_ifi = -1; __cur_thd->thnxtstp = NULL; }
     return;
    }

   /* if istep (within cur. itree inst. only) continue if different */
   if (__step_match_itp != NULL && __inst_ptr != __step_match_itp)
    continue;
   /* in same instance, make sure move to next line - keep exec ing */
   if (stp->stlin_cnt == __step_lini && (int) stp->stfnam_ind == __step_ifi)
    continue;
   /* hit step point, need to enter iact */
   break;
  }
 /* hit step stop know step non nil, suspend and return */
 /* set current step line in case in loop - most move to next line */
 __step_lini = stp->stlin_cnt;
 __step_ifi = (int) stp->stfnam_ind;
 /* stepped to something to stop at */
 if (stepped_to_new_loc(stp))
  {
   __last_stepitp = __inst_ptr;
   __last_steptskp = __cur_thd->assoc_tsk;
   __last_stepifi = (int) stp->stfnam_ind; 

    /* FIXME - is this __tr_s tracing ??? */
   __cvsim_msg("%s (%s line %d)", __msg_blditree(__xs, __last_stepitp,
    __last_steptskp), __in_fils[__last_stepifi], stp->stlin_cnt);
   if (__last_brktime != __simtime)
    {
     __cvsim_msg(" time %s\n", __to_timstr(__xs, &__simtime));
     __last_brktime = __simtime;
    }
   else __cvsim_msg("\n");
  }
 /* notice only change list location if print */
 if (__verbose_step)
  __prt_src_lines((int) stp->stfnam_ind, stp->stlin_cnt, stp->stlin_cnt);
 __single_step = FALSE;
 /* if more stepping, continue using istep itp matching if needed */
 if (__step_rep_cnt <= 1) __step_match_itp = NULL; 
 __verbose_step = FALSE;
 suspend_curthd(stp);
 /* even if interrupt (^c) received, doing again does not hurt */
 signal(SIGINT, SIG_IGN);
 /* when execing interactive command, never single stepped */
 __pending_enter_iact = TRUE;
 __iact_reason = IAER_STEP;  
}

/*
 * return T if stepped to new scope or new file
 */
static int stepped_to_new_loc(struct st_t *stp)
{
 if (__last_stepitp != __inst_ptr
  || __last_steptskp != __cur_thd->assoc_tsk
  || __last_stepifi != (int) stp->stfnam_ind
  || __last_brktime != __simtime) return(TRUE); 
 return(FALSE);
}

/*
 * break point and tracing version of execute one statement
 * also for executing non delay interactive statements
 */
extern struct st_t *__brktr_exec_1stmt(struct st_t *stp)
{
 register word val;
 int tmp, wlen;
 struct st_t *stp2;
 struct xstk_t *xsp;
 struct for_t *forp;
 struct if_t *ifinfo;
 struct expr_t *cntx;

again:
 /* notice must set location here - few cases where more than 1 stmt here */
 __slin_cnt = stp->stlin_cnt;
 __sfnam_ind = stp->stfnam_ind;
 __num_execstmts++;
 switch ((byte) stp->stmttyp) {
  case S_NULL: case S_STNONE: break;
  case S_FORASSGN:
   xsp = __eval_assign_rhsexpr(stp->st.spra.rhsx, stp->st.spra.lhsx);
   __exec2_proc_assign(stp->st.spra.lhsx, xsp->ap, xsp->bp);
   if (__st_tracing) tr_proc_assign(stp, xsp);
   __pop_xstk();
   stp = stp->stnxt;
   __num_addedexec++; 
   __num_execstmts++;
   goto again;
  case S_PROCA:
   xsp = __eval_assign_rhsexpr(stp->st.spra.rhsx, stp->st.spra.lhsx);
   __exec2_proc_assign(stp->st.spra.lhsx, xsp->ap, xsp->bp);
   if (__st_tracing) tr_proc_assign(stp, xsp);
   __pop_xstk();
   break;
  case S_NBPROCA:
   /* only non delay form non blocking assign exec here - implied #0 */
   xsp = __eval_assign_rhsexpr(stp->st.spra.rhsx, stp->st.spra.lhsx);
   if (__st_tracing) tr_nbproc_assign(stp, xsp);
   sched_nbproc_delay((struct delctrl_t *) NULL, xsp, stp);
   __pop_xstk();
   break;
  case S_RHSDEPROCA:
   /* this is continuation point for rhs form after block */
   wlen = wlen_(stp->st.spra.lhsx->szu.xclen);
   __exec2_proc_assign(stp->st.spra.lhsx, __cur_thd->th_rhswp,
    &(__cur_thd->th_rhswp[wlen]));
   if (__st_tracing)
    {
     /* here delay statement already displayed */
     __tr_msg("trace: %-7d %s = [%s] (saved rhs assign)\n", __slin_cnt,
      __msgexpr_tostr(__xs, stp->st.spra.lhsx),
      __xregab_tostr(__xs2, __cur_thd->th_rhswp, &(__cur_thd->th_rhswp[wlen]),
      stp->st.spra.lhsx->szu.xclen, stp->st.spra.rhsx));
    }
   /* must reset and free pending saved rhs over schedule */
   __my_free((char *) __cur_thd->th_rhswp, 2*wlen*WRDBYTES);
   __cur_thd->th_rhswp = NULL;
   __cur_thd->th_rhswlen = -1;
   __cur_thd->th_rhsform = FALSE;
   break;
  case S_IF:
   ifinfo = &(stp->st.sif);
   xsp = __eval_xpr(ifinfo->condx);
   /* condition T (1) only if at least 1, 1 */
   if (xsp->xslen <= WBITS)
    {
     /* SJM 07/20/00 - must convert to real if real */
     if (ifinfo->condx->is_real)
      {
       double d1;

       memcpy(&d1, xsp->ap, sizeof(double));
       tmp = (d1 != 0.0);
      }
     else tmp = ((xsp->ap[0] & ~xsp->bp[0]) != 0L);
    }
   else tmp = (__cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen)) == 1);
   __pop_xstk();
   if (__st_tracing)
    __tr_msg("trace: %-7d if (%s) [cond %d]\n", __slin_cnt,
     __msgexpr_tostr(__xs, ifinfo->condx), tmp);
   if (tmp) stp = ifinfo->thenst;
   else if (ifinfo->elsest != NULL) stp = ifinfo->elsest;
   else stp = stp->stnxt;
   return(stp);
  case S_CASE:
   /* notice Verilog cases cannot fall thru */
   if ((stp2 = exec_case(stp)) == NULL) break;
   return(stp2);
  case S_FOREVER:
   if (__st_tracing) __tr_msg("trace: %-7d forever\n", __slin_cnt);
   return(stp->st.swh.lpst);
  case S_REPSETUP:
   /* know repeat stmt follows rep setup */
   __num_addedexec++; 
   cntx = stp->stnxt->st.srpt.repx;
   xsp = __eval_xpr(cntx);
   /* SJM 04/02/02 - real count must be converted to word/int */
   if (cntx->is_real) __cnv_stk_fromreal_toreg32(xsp);
   if (xsp->xslen > WBITS) __sizchgxs(xsp, WBITS);

   if (xsp->ap[1] != 0L)
    {
     __last_stepitp = __cur_thd->th_itp;
     __last_steptskp = __cur_thd->assoc_tsk;
     __sgfwarn(645,
      "repeat loop in %s count has x/z expression value - loop skipped",
      __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
     val = 0;
    }
   else
    {
     /* SJM 04/02/02 - if repeat count signed and negative, never exec */ 
     if (cntx->has_sign && ((int) xsp->ap[0]) <= 0) val = 0;
     else val = xsp->ap[0];
    }
   __pop_xstk();
   /* notice count must be converted to unsigned with neg set to 0 */
   stp->stnxt->st.srpt.reptemp[__inum] = ++val;
   break;
  case S_REPEAT:
   val = --(stp->st.srpt.reptemp[__inum]); 
   if (__st_tracing)
    {
     __tr_msg("trace: %-7d repeat (%s) [count %u]\n", __slin_cnt,
      __msgexpr_tostr(__xs, stp->st.srpt.repx), val);
    }
   if (val == 0L) break;
   return(stp->st.srpt.repst);
  case S_WHILE:
   xsp = __eval_xpr(stp->st.swh.lpx);
   if (xsp->xslen <= WBITS)
    {
     if (stp->st.swh.lpx->is_real)
      { 
       double d1;

       memcpy(&d1, xsp->ap, sizeof(double));
       if (d1 != 0.0) tmp = 1; else tmp = 0;
       goto while_end;
      }
     val = xsp->bp[0];
     if ((xsp->ap[0] & ~val) != 0L)
      {
       if (val != 0) 
        {
         __sgfinform(403, "while in %s condition true but some bits x/z",
          __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
        }
       tmp = 1;
       goto while_end;
      }
     /* notice any 1 implies true so will not get here */
     if (val != 0)
      {
        __sgfinform(402,
        "while loop in %s terminating false condition value has x/z bits",
        __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
      } 
     tmp = 0;
     goto while_end;
    } 
   if ((tmp = __cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen))) == 1)
    {
     if (!vval_is0_(xsp->bp, xsp->xslen))
      {
       __sgfinform(403, "while in %s condition true but some bits x/z",
        __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
      }
     goto while_end;
    }
   /* notice any 1 implies true so will not get here */
   if (tmp == 3)
    {
     __sgfinform(402,
      "while loop in %s terminating false condition value has x/z bits",
      __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
    }
   
while_end:
   __pop_xstk();
   if (__st_tracing)
    __tr_msg("trace: %-7d while (%s) [cond: %d]\n", __slin_cnt,
     __msgexpr_tostr(__xs, stp->st.swh.lpx), tmp);
   if (tmp == 1) return(stp->st.swh.lpst);
   return(stp->stnxt);
  case S_WAIT:
   /* on true expression, returns true */
   if (exec_wait(stp)) return(stp->st.swait.lpst);
   /* is this unnecessary since action stmt points back to wait */
   __cur_thd->thnxtstp = stp;
   __stmt_suspend = TRUE;
   return(NULL);
  case S_FOR:
   /* when loop done for returns NULL as next statement else 1st body st. */
   forp = stp->st.sfor;
   if (!for_not_done(forp))
    break;
   return(forp->forbody);
  case S_REPDCSETUP:
   /* 10/27/00 SJM - added repeat form rhs ectl and nb proca ectl setup */
   /* next statment is s delctrl or one after if repeat cnt x/z or <= 0 */ 
   return(exec_rep_ectl_setup(stp));
  case S_DELCTRL:
   /* this returns F, for suspend, non blocking returns T */
   /* 10/27/00 SJM - for repeat rhs ectrl count x/z <= 0 assign */
   /* immediate so also return T */
   if (exec_dctrl(stp)) return(__cur_thd->thnxtstp);
   __stmt_suspend = TRUE;
   return(NULL);
  case S_NAMBLK:
   /* for function only, just continue in named block */
   if (__fcspi >= 0)
    {
     if (__st_tracing)
      {
       __tr_msg("trace: %-7d begin : %s\n", __slin_cnt,
        stp->st.snbtsk->tsksyp->synam);
      }
     return(stp->st.snbtsk->tskst);
    }
   /* know this is new down thread - know at least 1 statement */
   exec_namblk(stp);
   return(__cur_thd->thnxtstp);
  case S_UNBLK:
   if (__st_tracing) __tr_msg("trace: %-7d begin\n", __slin_cnt);
   return(stp->st.sbsts);
  case S_UNFJ:
   /* this is unnamed fork-join only */ 
   if (__st_tracing) __tr_msg("trace: %-7d fork\n", __slin_cnt);
   __sched_fork(stp);
   __cur_thd->thnxtstp = stp->stnxt;
   __stmt_suspend = TRUE;
   return(NULL);
  case S_TSKCALL:
   /* if system task, NULL will suspend, else continue in down thread */
   if ((stp2 = __exec_tskcall(stp)) == NULL) return(NULL);
   return(stp2);
  case S_QCONTA:
   if (stp->st.sqca->qcatyp == ASSIGN) __exec_qc_assign(stp, FALSE);
   else
    {
     /* force of reg, is like assign except overrides assign */
     if (stp->st.sqca->regform) __exec_qc_assign(stp, TRUE);
     else __exec_qc_wireforce(stp);
    }
   if (__st_tracing)
    {
     char s1[RECLEN], s2[RECLEN];

     strcpy(s2, "");  
     if (stp->st.sqca->qcatyp == ASSIGN)
      {
       strcpy(s1, "assign");
       if (__force_active) strcpy(s2, " [active force effect hidden]"); 
      }
     else
      {
       strcpy(s1, "force");
       if (__assign_active) strcpy(s2, " [assign value saved]");
      }
     __tr_msg("trace: %-7d %s %s = %s%s\n", __slin_cnt, s1, 
      __msgexpr_tostr(__xs, stp->st.sqca->qclhsx),
      __msgexpr_tostr(__xs2, stp->st.sqca->qcrhsx), s2);
    }
   /* --- DBG remove 
   __dmpmod_nplst(__inst_mod, TRUE); 
   --- */
   __force_active = FALSE;
   __assign_active = FALSE;
   break;
  case S_QCONTDEA:
   if (stp->st.sqcdea.qcdatyp == DEASSIGN) __exec_qc_deassign(stp, FALSE);
   else
    {
     if (stp->st.sqcdea.regform) __exec_qc_deassign(stp, TRUE);
     else __exec_qc_wirerelease(stp);
    } 

   if (__st_tracing)
    {
     char s1[RECLEN], s2[RECLEN];

     strcpy(s2, "");  
     if (stp->st.sqcdea.qcdatyp == DEASSIGN)
      {
       strcpy(s1, "deassign");
       if (__force_active) strcpy(s2, " [active force effect hidden]"); 
      }
     else
      {
       strcpy(s1, "release");
       if (__assign_active) strcpy(s2, " [assign value restored]");
      }
     __tr_msg("trace: %-7d %s %s%s\n", __slin_cnt, s1, 
      __msgexpr_tostr(__xs, stp->st.sqcdea.qcdalhs), s2);
    }
   __force_active = FALSE;
   __assign_active = FALSE;
   break;
  case S_CAUSE:
   exec_cause(stp);
   break;
  case S_DSABLE:
   if (__st_tracing)
    {
     __tr_msg("trace: %-7d disable %s;\n", __slin_cnt,
      __msgexpr_tostr(__xs, stp->st.sdsable.dsablx));
    }
   /* if function, disable means continue with statement after block */ 
   if (__fcspi >= 0) return(stp->st.sdsable.func_nxtstp);

   /* here - done - suspend off so nil will end thread */
   /* xmr disable will mark some thread and return F */ 
   if (__exec_disable(stp->st.sdsable.dsablx)) return(NULL);
   break;
  case S_GOTO:
   stp = stp->st.sgoto;
   /* debug to nil ok, just end of list */
   __num_addedexec++; 
   if (__st_tracing)
    __tr_msg("trace: %-7d --continue %s\n", __slin_cnt, 
     __bld_lineloc(__xs, stp->stfnam_ind, stp->stlin_cnt));
   __num_execstmts++;
   return(stp);
  /* do not trace break during break - may trace when execed */ 
  case S_BRKPT:
   /* returns T on need to break */
   if (__process_brkpt(stp)) return(stp); 

   /* not a break for some reason - restore stmt type and exec 1 stmt */
   /* if bp halt off 2nd time through after break, this execs */
   stp->stmttyp = stp->rl_stmttyp;
   stp2 = __brktr_exec_1stmt(stp);
   stp->stmttyp = S_BRKPT;
   return(stp2);
  default: __case_terr(__FILE__, __LINE__);
 }
 return(stp->stnxt);
}

/*
 * evaluate an procedural assign rhs expression and convert to form
 * needed for assignment
 * handles real conversion and size changes - never z widening
 * know returned stack width always exactly matches lhs width
 */
extern struct xstk_t *__eval_assign_rhsexpr(register struct expr_t *xrhs,
 register struct expr_t *xlhs)
{
 register struct xstk_t *xsp;

 xsp = __eval_xpr(xrhs);
 if (xlhs->is_real)
  { 
   /* needed: think passing packed bit does not work on all compilers ? */ 
   if (!xrhs->is_real) __cnv_stk_fromreg_toreal(xsp, (xrhs->has_sign == 1));
  }
 else
  { 
   /* handle rhs preparation of reals - then assign is just copy for reals */
   if (xrhs->is_real) __cnv_stk_fromreal_toreg32(xsp);
   if (xsp->xslen != xlhs->szu.xclen) __sizchgxs(xsp, xlhs->szu.xclen);
  }
 return(xsp);
}

/*
 * evaulate task assign - only different if form of various flags 
 */
static void eval_tskassign_rhsexpr(register struct xstk_t *xsp,
 register int lhsreal, register int lhswid, register int rhsreal,
 register int rhssign)
{
 if (lhsreal)
  { 
   /* think passing packed bit does not work on all compilers ? */ 
   if (!rhsreal) __cnv_stk_fromreg_toreal(xsp, rhssign);
  }
 else
  { 
   /* handle rhs preparation of reals - then assign is just copy for reals */
   if (rhsreal) __cnv_stk_fromreal_toreg32(xsp);
   if (xsp->xslen != lhswid) __sizchgxs(xsp, lhswid);
  }
}

/*
 * trace an assignment statement
 * notice this expects rhs value to be on top of stack (caller pops)
 *
 * ok to use rgab_tostr here since know __exprline can not be in use before
 * statement execution begins
 */
static void tr_proc_assign(struct st_t *stp, struct xstk_t *xsp)
{
 struct expr_t *xrhs;

 __cur_sofs = 0;
 __dmp_proc_assgn((FILE *) NULL, stp, (struct delctrl_t *) NULL, FALSE);
 __exprline[__cur_sofs] = 0; 
 __trunc_exprline(TRTRUNCLEN, FALSE);
 __tr_msg("trace: %-7d %s", __slin_cnt, __exprline);
 xrhs = stp->st.spra.rhsx;
 /* if rhs is number value is obvious, else print value that was assigned */
 /* any conversion to lhs already made so expr. info from lhs */ 
 if (xrhs->optyp != NUMBER && xrhs->optyp != REALNUM)
  {
   __tr_msg(" [%s]\n", __xregab_tostr(__xs, xsp->ap, xsp->bp, xsp->xslen,
    stp->st.spra.lhsx));
  }
 else __tr_msg("\n");
 __cur_sofs = 0;
}

/*
 * trace an non blocking assignment statement
 * notice this expects rhs value to be on top of stack (caller pops)
 *
 * ok to use rgab_tostr here since know __exprline can not be in use before
 * statement execution begins
 */
static void tr_nbproc_assign(struct st_t *stp, struct xstk_t *xsp)
{
 struct expr_t *xrhs;

 __cur_sofs = 0;
 __dmp_nbproc_assgn((FILE *) NULL, stp, (struct delctrl_t *) NULL);
 __exprline[__cur_sofs] = 0; 
 __trunc_exprline(TRTRUNCLEN, FALSE);
 __tr_msg("trace: %-7d %s", __slin_cnt, __exprline);
 xrhs = stp->st.spra.rhsx;
 if (xrhs->optyp != NUMBER && xrhs->optyp != REALNUM)
  {
   __tr_msg(" [%s]\n", __xregab_tostr(__xs, xsp->ap, xsp->bp,
    xsp->xslen, xrhs));
  }
 else __tr_msg("\n");
 __cur_sofs = 0;
}

/*
 * execute repeat event setup (rhs nb ectl proca or rhs ectl delay  
 * know rhs DEL CTRL stmt with repeat form rhs ev control follows
 * only can be rhs ev control or rhs non blocking assign ev ctrl
 */
static struct st_t *exec_rep_ectl_setup(struct st_t *stp)
{
 register struct xstk_t *xsp;
 register struct st_t *nxtstp, *astp;
 struct delctrl_t *rdctp;
 word val;

 __num_addedexec++; 
 nxtstp = stp->stnxt;
 /* DBG RELEASE remove --- */
 if (nxtstp->stmttyp != S_DELCTRL) __misc_terr(__FILE__, __LINE__);
 /* --- */
 rdctp = nxtstp->st.sdc;
 xsp = __eval_xpr(rdctp->repcntx);
 /* SJM 04/02/02 - real count must be converted to word/int */
 if (rdctp->repcntx->is_real) __cnv_stk_fromreal_toreg32(xsp);

 /* FIXME ??? - although know WBITS wide, should use bp here */
 if (xsp->ap[1] != 0L)
  {
   __sgfwarn(645,
    "repeat event control in %s count has x/z value - no wait for event",
    __msg_blditree(__xs, __cur_thd->th_itp, __cur_thd->assoc_tsk));
   __pop_xstk();

immed_ectl_exec:     
   astp = nxtstp->st.sdc->actionst;
   xsp = __eval_assign_rhsexpr(astp->st.spra.rhsx, astp->st.spra.lhsx);
   if (astp->stmttyp == S_NBPROCA)
    {
     /* case 1: NB assign - becomes no delay NB assign form */ 
     sched_nbproc_delay(NULL, xsp, astp);
    }
   else
    {
     /* case 2: rhs repeat event control - treat as simple proca */ 
     __exec2_proc_assign(astp->st.spra.lhsx, xsp->ap, xsp->bp);
    }
   __pop_xstk();
   /* continuation statment is one after S DELCTRL since no ev ctrl */
   /* in this case */
   return(nxtstp->stnxt);
  }
 /* if signed and <= 0, or word equal to 0, becomes immediate assign */
 /* SJM 04/02/02 - need to use unsigned counter and convert neg to 0 */
 if (rdctp->repcntx->has_sign && (int) xsp->ap[0] <= 0) val = 0;
 else val = xsp->ap[0];
 __pop_xstk();
 if (val == 0) goto immed_ectl_exec;

 /* val now number of edges (if 1 same as normal rhs ectrl */
 /* notice, here never exec unless at least one so do not need inc */
 rdctp->dce_repcnts[__inum] = val;
 return(nxtstp);
}

/*
 * execute a delay control indicator
 * notice this arms or schedules something - caller suspends thread
 * this return T if non blocking assign needs to not schedule
 */
static int exec_dctrl(struct st_t *stp)
{
 int bytes, wlen;
 word *wp;
 struct delctrl_t *dctp;
 struct xstk_t *xsp;
 struct st_t *astp;

 dctp = stp->st.sdc;
 if (__st_tracing)
  {
   __evtr_resume_msg();
   __cur_sofs = 0;
   if (dctp->actionst == NULL || dctp->dctyp == DC_RHSEVENT
    || dctp->dctyp == DC_RHSDELAY) __dmp_dctrl((FILE *) NULL, dctp);
   else
    {
     if (dctp->dctyp == DC_EVENT) addch_('@'); else addch_('#');
     __dmp_dcxpr((FILE *) NULL, dctp->dc_du, dctp->dc_delrep);
    }
   __trunc_exprline(TRTRUNCLEN, FALSE);
   __tr_msg("trace: %-7d %s\n", __slin_cnt, __exprline);
   __cur_sofs = 0;
  }
 /* for all but non blocking assign block - continue with action statement */
 if (dctp->actionst == NULL) __cur_thd->thnxtstp = stp->stnxt;
 else __cur_thd->thnxtstp = dctp->actionst;

 switch ((byte) dctp->dctyp) {
  case DC_DELAY:
   sched_proc_delay(dctp, (word *) NULL, -1);
   break;
  case DC_EVENT:
   arm_event_dctrl(dctp, (word *) NULL, -1);
   break; 
  case DC_RHSDELAY: case DC_RHSEVENT:
   /* 10/28/00 SJM - only rhs event either blocking or non blocking */
   /* can have repeat form */
   astp = dctp->actionst;
   /* rhs # delay or event ctrl */
   /* -- DBG remove
   if (astp == NULL || (astp->stmttyp != S_RHSDEPROCA
    && astp->stmttyp != S_NBPROCA)) __arg_terr(__FILE__, __LINE__);
   --- */ 

   /* evaluate rhs and schedule as usual */
   /* notice this depends on contiguous xsp a and b parts */
   xsp = __eval_assign_rhsexpr(astp->st.spra.rhsx, astp->st.spra.lhsx);

   if (astp->stmttyp == S_NBPROCA)
    {
     /* for non blocking assign - must not exec assign - event processing */
     /* routine does that, must continue after actionst if can else nxt */
     if (dctp->actionst != NULL && dctp->actionst->stnxt != NULL)
      __cur_thd->thnxtstp = dctp->actionst->stnxt;
     else __cur_thd->thnxtstp = stp->stnxt;
     
     if (dctp->dctyp == DC_RHSDELAY) sched_nbproc_delay(dctp, xsp, astp);
     else arm_nbevent_dctrl(dctp, xsp, astp);
     __pop_xstk();
     return(TRUE);
    }
   /* continuation point for rhs delay form is action statement if exists */
   /* that is same as normal delay control */

   /* if blocking allocate and store - no inertial problems for blocking */
   /* SJM - 01/12/00 - wlen_ omitted so here was large memory leak */
   /*                  was only freeing 4/32 percent of bytes */
   wlen = wlen_(astp->st.spra.lhsx->szu.xclen);
   bytes = 2*WRDBYTES*wlen;
   
   wp = (word *) __my_malloc(bytes);
   memcpy(wp, xsp->ap, bytes); 

   if (dctp->dctyp == DC_RHSDELAY) sched_proc_delay(dctp, wp, wlen);
   else arm_event_dctrl(dctp, wp, wlen);
   __pop_xstk();
   break;
  default: __case_terr(__FILE__, __LINE__);
 }
 return(FALSE);
}

/*
 * schedule procedural delay thread simple prefix timing delay
 *
 * must continue after wake up with same thread (contents?)
 * before call statement set to statement to exec after wake up
 * also handles rhs delay form
 *
 * notice on disable event canceled and any rhs value free but that is
 * all that is needed
 */
static void sched_proc_delay(struct delctrl_t *dctp, word *wp, int wlen)
{
 register i_tev_ndx tevpi;
 register struct tev_t *tevp;
 word64 t, schtim;
 struct st_t *stp;

 /* this can not be edge delay or syntax error before here */
 __get_del(&t, dctp->dc_du, dctp->dc_delrep);  
 schtim = __simtime + t;
 alloc_tev_(tevpi, TE_THRD, __inst_ptr, schtim);
 /* set the associate event - after return, __cur_thd will be new */
 __cur_thd->thdtevi = tevpi;
 /* restart current - will block after here and change threads */
 tevp = &(__tevtab[tevpi]);
 tevp->tu.tethrd = __cur_thd;
 /* if rhs delay form, set values */ 
 if (wp != NULL)
  {
   __cur_thd->th_rhsform = TRUE;
   __cur_thd->th_rhswp = wp;
   __cur_thd->th_rhswlen = wlen;  
  }

 if (__ev_tracing)
  {
   char s1[RECLEN], vs2[10];

   __evtr_resume_msg();
   if (wp == NULL) strcpy(vs2, ""); else strcpy(vs2, "(rhs)");
   stp = tevp->tu.tethrd->thnxtstp;
   __tr_msg("-- scheduling delay resume%s at %s for time %s\n",
    vs2, __bld_lineloc(s1, stp->stfnam_ind, stp->stlin_cnt),
    __to_timstr(__xs, &(tevp->etime)));
  }
 /* notice that procedural #0 (does not need to be rhs assign form) */
 /* done after all normal events */
 if (t == 0ULL)
  {
   if (__debug_flg && __ev_tracing)
    {
     __tr_msg("sched: adding #0 %s event to list end\n",
      __to_tetyp(__xs, tevp->tetyp));
    }
   /* notice pound 0 only added from current time events */
   if (__p0_te_hdri == -1) __p0_te_hdri = __p0_te_endi = tevpi;
   else { __tevtab[__p0_te_endi].tenxti = tevpi; __p0_te_endi = tevpi; }
  }
 /* if non blocking procedural assign, insert in normal moved to #0 later */
 else __insert_event(tevpi);
}

/*
 * schedule non blocking procedural assign simple prefix timing delay
 *
 * this is simple because of strange non hardware related semantics
 * every time a non blocking delay assigned is executed, just compute
 * delay and schedule - can have >1 events per unit or per statement
 * but just schedule and forget
 */
static void sched_nbproc_delay(struct delctrl_t *dctp, struct xstk_t *xsp,
 struct st_t *stp)
{
 i_tev_ndx tevpi;
 word64 t, schtim;

 /* if no delay form, schedule at end of currnt time #0s */
 if (dctp == NULL) t = 0ULL;
 /* error before here if edge dependent delay */
 else __get_del(&t, dctp->dc_du, dctp->dc_delrep);  
 schtim = __simtime + t;

 if (__ev_tracing)
  {
   char s1[RECLEN], s2[RECLEN], s3[RECLEN];

   __evtr_resume_msg();
   __tr_msg(
    "-- scheduling delay form non blocking assign line %s now %s in %s:\n",
    __bld_lineloc(s1, stp->stfnam_ind, stp->stlin_cnt),
    __to_timstr(s2, &__simtime), __msg2_blditree(s3, __inst_ptr));
   __tr_msg(" NB SCHEDULE TO NEW VALUE %s AT TIME %s\n",
    __xregab_tostr(s1, xsp->ap, xsp->bp, stp->st.spra.rhsx->szu.xclen,
     stp->st.spra.rhsx), __to_timstr(s2, &schtim));
  }

 /* build the disable remove list for possibly multiple active nb forms */
 tevpi = __bld_nb_tev(stp, xsp, schtim); 
 /* final step is inserting event in list */
 /* no dleay form becomes #0 schedule for assign event here */
 if (t == 0ULL)
  {
   if (__debug_flg && __ev_tracing)
    {
     __tr_msg("sched: adding #0 %s event to list end\n",
      __to_tetyp(__xs, __tevtab[tevpi].tetyp));
    }
   /* notice pound 0 only added from current time events */
   if (__p0_te_hdri == -1) __p0_te_hdri = __p0_te_endi = tevpi;
   else { __tevtab[__p0_te_endi].tenxti = tevpi; __p0_te_endi = tevpi; }
  }
 /* if non blocking procedural assign, insert in normal moved to #0 later */
 else __insert_event(tevpi);
}

/*
 * build and emit trace message for non blocking schedule or trigger
 * notice these are not inertial - just keep scheduling
 * never cancel or re-schedule
 *
 * SJM 08/08/99 - change so if lhs expr (maybe concat) has non constant
 * bit selects copy and then evaluate variable indices to numbers
 * and change copied expr.
 *
 * BEWARE - code here and in many places assumes numeric expressions
 * folded to number or IS number by here
 */
extern i_tev_ndx __bld_nb_tev(struct st_t *stp, struct xstk_t *xsp,
 word64 schtim)
{
 register struct tenbpa_t *nbpap;
 register word *wp;
 i_tev_ndx tevpi;
 int wlen; 
 struct expr_t *lhsxp;

 alloc_tev_(tevpi, TE_NBPA, __inst_ptr, schtim);
 nbpap = (struct tenbpa_t *) __my_malloc(sizeof(struct tenbpa_t)); 
 __tevtab[tevpi].tu.tenbpa = nbpap;
 wlen = wlen_(stp->st.spra.lhsx->szu.xclen);
 wp = (word *) __my_malloc(2*wlen*WRDBYTES);

 memcpy(wp, xsp->ap, 2*wlen*WRDBYTES);

 nbpap->nbawp = wp; 
 nbpap->nbastp = stp;

 /* copy expr. if needed */
 /* BEWARE - code in many places assumes numeric expressions folded to */ 
 /* number or IS number by here */
 if (!__lhsexpr_var_ndx(stp->st.spra.lhsx)) nbpap->nblhsxp = NULL;
 else
  {
   /* notice - know will have same width as stp lhsx */
   lhsxp = __sim_copy_expr(stp->st.spra.lhsx);
   __eval_lhsexpr_var_ndxes(lhsxp);
   nbpap->nblhsxp = lhsxp; 
  }
 /* caller sets dctp if needed */
 nbpap->nbdctp = NULL;
 return(tevpi);
}

/*
 * return T if expression contains non constant bit select index
 *
 * this assume only one level concats but maybe should
 */
extern int __lhsexpr_var_ndx(register struct expr_t *xp)
{
 switch ((byte) xp->optyp) {
  case GLBREF: case ID:
   break;
  case PARTSEL:
   /* part select always constant */
   break;
  case LSB:
   if (xp->ru.x->optyp == NUMBER || xp->ru.x->optyp == ISNUMBER) break;
   return(TRUE);
  case LCB:
   {
    register struct expr_t *catxp;

    for (catxp = xp->ru.x; catxp != NULL; catxp = catxp->ru.x)
     {
      /* if var index must copy entire expr. */
      if (__lhsexpr_var_ndx(catxp->lu.x)) return(TRUE);
     }
   } 
   break;
  default: __case_terr(__FILE__, __LINE__);
 }
 return(FALSE);
}

/*
 * evaluate any variable indices to constants
 *
 * this is guts of LRM non-blocking assign algorithm - for any variable
 * bit index eval and convert to constant
 *
 * this mangles expr but since copied and free when non blocking assign
 * done still works
 *
 * assumes only one level concats but maybe should
 */
extern void __eval_lhsexpr_var_ndxes(register struct expr_t *xp)
{
 int biti;
 struct expr_t *idndp;
 struct net_t *np;
 struct expr_t *ndx;

 switch ((byte) xp->optyp) {
  case GLBREF: case ID: break;
  case PARTSEL:
   /* part select always constant */
   break;
  case LSB:
   if (xp->ru.x->optyp != NUMBER && xp->ru.x->optyp != ISNUMBER)
    {
     idndp = xp->lu.x;
     np = idndp->lu.sy->el.enp;
     /* can be either constant or expr. - both handled in comp. */
     biti = __comp_ndx(np, xp->ru.x);
     /* out of range is x as index */
     if (biti == -1) ndx = __bld_rng_numxpr(ALL1W, ALL1W, WBITS);
     else ndx = __bld_rng_numxpr((word) biti, 0, WBITS);
     __free_xtree(xp->ru.x);
     xp->ru.x = ndx;
    }
   /* if constant (even IS) index, nothing to do */ 
   break;
  case LCB:
   {
    register struct expr_t *catxp;

    for (catxp = xp->ru.x; catxp != NULL; catxp = catxp->ru.x)
     {
      /* if var index must copy entire expr. */
      __eval_lhsexpr_var_ndxes(catxp->lu.x);
     }
   } 
   break;
  default: __case_terr(__FILE__, __LINE__);
 }
}

/*
 * arm an event delay control - know already set up
 * know current thread set to continuation point here
 * know current thread will be blocked waiting for this 1 event
 *
 * notice may be triggered from other thread (init/always) in same inst.
 * but continuation is here
 * here arming ref. instance even though only change of target wire will
 * trigger for xmr or col. case
 */
static void arm_event_dctrl(struct delctrl_t *dctp, register word *wp,
 int wlen)
{
 register i_tev_ndx tevpi;
 struct tev_t *tevp;
 struct st_t *stp;

 /* build after trigger fires, startup event */
 /* notice this event record is not linked onto any event list for now */
 alloc_tev_(tevpi, TE_THRD, __inst_ptr, __simtime);
 /* link event back to thread */
 __cur_thd->thdtevi = tevpi;
 tevp = &(__tevtab[tevpi]);
 tevp->tu.tethrd = __cur_thd;

 /* if rhs delay form, set values */ 
 if (wp != NULL)
  {
   __cur_thd->th_rhsform = TRUE;
   __cur_thd->th_rhswp = wp;
   __cur_thd->th_rhswlen = wlen;
  }

 if (__debug_flg && __st_tracing)
  {
   stp = tevp->tu.tethrd->thnxtstp;
   __tr_msg("-- arming event thread %s itree loc. %s statement at %s\n",
    __cur_thd->th_itp->itip->isym->synam, __inst_ptr->itip->isym->synam,
    __bld_lineloc(__xs, stp->stfnam_ind, stp->stlin_cnt));
  }
 /* RELEASE remove --
 if (__debug_flg)
  __dmp_dcemsg(dctp, "setting dce to event");
 --- */

 /* if rexecuting task, algorithm is to cancel previous pending */
 /* delay control and emit warning */
 if (dctp->dceschd_tevs[__inum] != -1)
  {
   if (__cur_thd->assoc_tsk == NULL)
    {
     stp = tevp->tu.tethrd->thnxtstp;
     __sgfwarn(635, 
      "INTERNAL BUG? - in %s cancel and rearm of event control to resume at %s",
      __msg2_blditree(__xs, __inst_ptr), __bld_lineloc(__xs2, stp->stfnam_ind,
      stp->stlin_cnt));
    }
   else
    {
     stp = tevp->tu.tethrd->thnxtstp;
     __sgfwarn(635, 
      "when reexecuting task %s cancel and rearm of event control to resume at %s",
      __msg_blditree(__xs, __inst_ptr, __cur_thd->assoc_tsk),
      __bld_lineloc(__xs2, stp->stfnam_ind, stp->stlin_cnt));
    }
  }

 /* DBG remove -- */
 if (__cur_thd->th_dctp != NULL) __misc_terr(__FILE__, __LINE__);
 /* --- */

 /* notice simply linking event on the scheduled list enables the ev ctrl */
 dctp->dceschd_tevs[__inum] = tevpi;
 __cur_thd->th_dctp = dctp;
 /* handle any tracing */
 if (__ev_tracing)
  {
   char vs2[10];

   stp = tevp->tu.tethrd->thnxtstp;
   if (wp == NULL) strcpy(vs2, ""); else strcpy(vs2, "(rhs)");
   __tr_msg("-- event control suspend%s to resume line %s\n", 
    vs2, __bld_lineloc(__xs, stp->stfnam_ind, stp->stlin_cnt));
  }
}

/*
 * print a dctp message for debugging
 */
extern void __dmp_dcemsg(struct delctrl_t *dctp, char *dcemsg)
{
 char s1[RECLEN];

 if (dctp->actionst != NULL)
  __bld_lineloc(s1, dctp->actionst->stfnam_ind, dctp->actionst->stlin_cnt);
 else strcpy(s1, "<none>");
 __dbg_msg("%s: at %p of type %s instance %d(%s) iact=%d stmt. %s\n", dcemsg, 
  dctp, __to_dcenam(__xs, dctp->dctyp), __inum,
  __msg2_blditree(__xs2, __inst_ptr), dctp->dc_iact, s1);
}

/*
 * arm a non blocking assign delay control
 * 
 * multiple allowed just add each new tev to end
 * LOOKATME - notice current scheme requires linear traversal to list end.
 */
static void arm_nbevent_dctrl(struct delctrl_t *dctp, struct xstk_t *xsp,
 struct st_t *stp)
{
 register i_tev_ndx tevp2i;
 i_tev_ndx tevpi;
 struct tev_t *tevp;
 char s1[RECLEN], s2[RECLEN], s3[RECLEN]; 

 /* DBG remove -- */
 if (__ev_tracing)
  {
   __evtr_resume_msg();
    __tr_msg(
     "-- arming event control non blocking assign line %s now %s in %s:\n",
     __bld_lineloc(s1, stp->stfnam_ind, stp->stlin_cnt),
     __to_timstr(s3, &__simtime), __msg2_blditree(s2, __inst_ptr));

   __tr_msg("   EVENT TRIGGER ARM NEW VALUE %s\n",
    __xregab_tostr(s1, xsp->ap, xsp->bp, stp->st.spra.lhsx->szu.xclen,
    stp->st.spra.rhsx));
  }
 /* --- */
 tevpi = __bld_nb_tev(stp, xsp, __simtime); 
 tevp = &(__tevtab[tevpi]);
 tevp->nb_evctrl = TRUE;
 /* for event control form need to set dctp field */
 tevp->tu.tenbpa->nbdctp = dctp;
 if ((tevp2i = dctp->dceschd_tevs[__inum]) != -1)
  {
   /* could save end pointer if too slow ? */
   for (; __tevtab[tevp2i].tenxti != -1; tevp2i = __tevtab[tevp2i].tenxti) ;
   __tevtab[tevp2i].tenxti = tevpi;
   /* ??? LOOKATME is this needed */
   __tevtab[tevpi].tenxti = -1;
  }
 else dctp->dceschd_tevs[__inum] = tevpi;
}

/*
 * execute a simple (not casex and casez) case statement
 */
static struct st_t *exec_case(struct st_t *stp)
{
 register word aw, bw;
 register struct xstk_t *itemxsp;
 register struct exprlst_t *xplp;
 register struct csitem_t *csip;
 int selxlen, selwlen, i; 
 struct xstk_t *selxsp;
 struct csitem_t *dflt_csip;

 /* SJM 12/12/03 - must treat all 3 case types as special case if any */
 /* of select or case item exprs real */
 if (stp->st.scs.csx->is_real || stp->st.scs.csx->cnvt_to_real)
  {
   return(exec_real_case(stp));
  }

 if (stp->st.scs.castyp == CASEX) return(exec_casex(stp));
 else if (stp->st.scs.castyp == CASEZ) return(exec_casez(stp));

 /* compute the case type - determines operator to use */
 selxsp = __eval_xpr(stp->st.scs.csx);

 /* if result of selector is not as wide as needed widen */
 /* case needs w bits width but selector is wire < w */
 if (selxsp->xslen != (int) stp->st.scs.maxselwid)
  __sizchgxs(selxsp, (int) stp->st.scs.maxselwid);
 selxlen = selxsp->xslen; 
 if (__st_tracing) tr_case_st(selxsp, (stp->st.scs.csx->has_sign == 1));
 dflt_csip = stp->st.scs.csitems;
 csip = dflt_csip->csinxt;

 /* case case 1: fits in one word */
 if (selxlen <= WBITS)
  {
   aw = selxsp->ap[0];
   bw = selxsp->bp[0];
   for (; csip != NULL; csip = csip->csinxt)
    {
     for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
      {
       itemxsp = __eval2_xpr(xplp->xp);

       /* SJM 12/12/03 - never can be real here using new all if any code */

       /* no conversion needed becaause know item may be too narrow only */
       if (((aw ^ itemxsp->ap[0]) | (bw ^ itemxsp->bp[0])) == 0)
        { __pop_xstk(); __pop_xstk(); return(csip->csist); }
       __pop_xstk();
      }
    }
   __pop_xstk();
   if (dflt_csip->csist != NULL) return(dflt_csip->csist);
   return(NULL);
  }
 /* case case 2: wider than 1 word */
 selwlen = wlen_(selxlen);
 for (; csip != NULL; csip = csip->csinxt)
  {
   for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
    {
     itemxsp = __eval2_xpr(xplp->xp);

     /* SJM 12/12/03 - never can be real here using new all if any code */

     if (selxlen != itemxsp->xslen) __sizchgxs(itemxsp, selxlen);
     for (i = 0; i < selwlen; i++)
      {
       if (((selxsp->ap[i] ^ itemxsp->ap[i])
        | (selxsp->bp[i] ^ itemxsp->bp[i])) != 0) goto nxt_x;
      }
     __pop_xstk();
     __pop_xstk();
     return(csip->csist);

nxt_x:
     __pop_xstk();
    }
  }  
 __pop_xstk();
 if (dflt_csip->csist != NULL) return(dflt_csip->csist);
 return(NULL);
}

/*
 * special case routine to exec a case where any expr real
 *
 * SJM 12/12/03 - was converting real to word for cases with real but
 * think that is wrong (although 2001 LRM does not say exactly) so now
 * if any of case select or case item real, all compares real
 */
static struct st_t *exec_real_case(struct st_t *stp)
{
 register struct xstk_t *itemxsp;
 register struct exprlst_t *xplp;
 register struct csitem_t *csip;
 double d1, d2;
 struct xstk_t *selxsp;
 struct csitem_t *dflt_csip;

 /* warning if casex or casez with real since no effect */
 if (stp->st.scs.castyp == CASEX || stp->st.scs.castyp == CASEZ)
  {
   __sgfwarn(3113,
    "select or case item expression real - casex/casez no effect");
  }

 /* compute the case type - determines operator to use */
 selxsp = __eval_xpr(stp->st.scs.csx);

 /* if select expr not real convert it */
 if (stp->st.scs.csx->cnvt_to_real)
  __cnv_stk_fromreg_toreal(selxsp, stp->st.scs.csx->has_sign);

 if (__st_tracing) tr_case_st(selxsp, (stp->st.scs.csx->has_sign == 1));

 dflt_csip = stp->st.scs.csitems;
 csip = dflt_csip->csinxt;
 memcpy(&d1, selxsp->ap, sizeof(double));

 for (; csip != NULL; csip = csip->csinxt)
  {
   for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
    {
     itemxsp = __eval2_xpr(xplp->xp);

     /* if case item expr not real convert it */
     if (xplp->xp->cnvt_to_real)
      {
       __cnv_stk_fromreg_toreal(itemxsp, xplp->xp->has_sign);
      } 
     memcpy(&d2, itemxsp->ap, sizeof(double));

     if ((d2 - d1) > -EPSILON && (d2 - d1) < EPSILON)
      { __pop_xstk(); __pop_xstk(); return(csip->csist); }
     __pop_xstk();
    }
  }
 __pop_xstk();
 if (dflt_csip->csist != NULL) return(dflt_csip->csist);
 return(NULL);
}

/*
 * trace a case (for any of case/casex/casez) 
 */
static void tr_case_st(struct xstk_t *selxsp, int cas_sign)
{
 __tr_msg("trace: %-7d -- [selector: %d'h%s]\n",
  __slin_cnt, selxsp->xslen, __regab_tostr(__xs, selxsp->ap, selxsp->bp,
  selxsp->xslen, BHEX, cas_sign));
}

/*
 * execute a casex case statement
 */
static struct st_t *exec_casex(struct st_t *stp)
{
 register word aw, bw;
 register struct xstk_t *itemxsp;
 register struct csitem_t *csip;
 register struct exprlst_t *xplp;
 int selxlen, selwlen, i;
 struct xstk_t *selxsp;
 struct csitem_t *dflt_csip;

 /* compute the case type - determines operator to use */
 selxsp = __eval_xpr(stp->st.scs.csx);

 /* if expression real, convert to 32 bit reg */
 if (stp->st.scs.csx->is_real) __cnv_stk_fromreal_toreg32(selxsp);

 /* if result of selector is not as wide as needed widen */
 /* case needs w bits width but selector is wire < w */
 if (selxsp->xslen != (int) stp->st.scs.maxselwid)
   __sizchgxs(selxsp, (int) stp->st.scs.maxselwid);
 selxlen = selxsp->xslen; 
 if (__st_tracing) tr_case_st(selxsp, (stp->st.scs.csx->has_sign == 1));

 dflt_csip = stp->st.scs.csitems;
 csip = dflt_csip->csinxt;

 /* case case 1: fits in one word */
 if (selxlen <= WBITS)
  {
   aw = selxsp->ap[0];
   bw = selxsp->bp[0];
   for (; csip != NULL; csip = csip->csinxt)
    {
     for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
      {
       itemxsp = __eval2_xpr(xplp->xp);

       /* SJM 12/12/03 - never can be real here using new all if any code */

       /* no conversion needed becaause know item may be too narrow only */
       /* must 0 any don't care bits with either x/z bit 0 mask */
       if ((((aw ^ itemxsp->ap[0]) | (bw ^ itemxsp->bp[0]))
        & ~(bw | itemxsp->bp[0])) == 0)
        { __pop_xstk(); __pop_xstk(); return(csip->csist); }
       __pop_xstk();
      }
    }
   __pop_xstk();
   if (dflt_csip->csist != NULL) return(dflt_csip->csist);
   return(NULL);
  }
 /* case case 2: wider than 1 word */
 selwlen = wlen_(selxlen);
 for (; csip != NULL; csip = csip->csinxt)
  {
   for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
    {
     itemxsp = __eval2_xpr(xplp->xp);

     /* SJM 12/12/03 - never can be real here using new all if any code */

     if (selxlen != itemxsp->xslen) __sizchgxs(itemxsp, selxlen);
     for (i = 0; i < selwlen; i++)
      {
/* SJM 01/08/99 - WRONG - for wide if == 0 always matches first */
       if ((((selxsp->ap[i] ^ itemxsp->ap[i])
        | (selxsp->bp[i] ^ itemxsp->bp[i]))
        & ~(selxsp->bp[i] | itemxsp->bp[i])) != 0) goto nxt_x;
      }
     __pop_xstk();
     __pop_xstk();
     return(csip->csist);

nxt_x:
     __pop_xstk();
    }
  }
 __pop_xstk();
 if (dflt_csip->csist != NULL) return(dflt_csip->csist);
 return(NULL);
}

/*
 * execute a casez case statement
 */
static struct st_t *exec_casez(struct st_t *stp)
{
 register word aw, bw;
 register struct xstk_t *itemxsp;
 register struct csitem_t *csip;
 register struct exprlst_t *xplp;
 register word mask;
 int selxlen, selwlen, i;
 struct xstk_t *selxsp;
 struct csitem_t *dflt_csip;

 /* compute the case type - determines operator to use */
 selxsp = __eval_xpr(stp->st.scs.csx);

 /* if expression real, convert to 32 bit reg */
 if (stp->st.scs.csx->is_real) __cnv_stk_fromreal_toreg32(selxsp);

 /* if result of selector is not as wide as needed widen */
 /* case needs w bits width but selector is wire < w */
 if (selxsp->xslen != (int) stp->st.scs.maxselwid)
  __sizchgxs(selxsp, (int) stp->st.scs.maxselwid);
 selxlen = selxsp->xslen; 
 if (__st_tracing) tr_case_st(selxsp, (stp->st.scs.csx->has_sign == 1));

 dflt_csip = stp->st.scs.csitems;
 csip = dflt_csip->csinxt;

 /* case case 1: fits in one word */
 if (selxlen <= WBITS)
  {
   aw = selxsp->ap[0];
   bw = selxsp->bp[0];
   for (; csip != NULL; csip = csip->csinxt)
    {
     for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
      {
       itemxsp = __eval_xpr(xplp->xp);

       /* SJM 12/12/03 - never can be real here using new all if any code */

       /* no conversion needed becaause know item may be too narrow only */
       /* must 0 any don't care bits z bits in either */
       mask = (aw | ~bw) & (itemxsp->ap[0] | ~itemxsp->bp[0]);
       if ((((aw ^ itemxsp->ap[0]) | (bw ^ itemxsp->bp[0])) & mask) == 0)
        { __pop_xstk(); __pop_xstk(); return(csip->csist); }
       __pop_xstk();
      }
    }
   __pop_xstk();
   if (dflt_csip->csist != NULL) return(dflt_csip->csist);
   return(NULL);
  }
 /* case case 2: wider than 1 word */
 selwlen = wlen_(selxlen);
 for (; csip != NULL; csip = csip->csinxt)
  {
   for (xplp = csip->csixlst; xplp != NULL; xplp = xplp->xpnxt)
    {
     itemxsp = __eval_xpr(xplp->xp);

     /* convert if real */
     /* SJM 12/12/03 - never can be real here using new all if any code */

     if (selxlen != itemxsp->xslen) __sizchgxs(itemxsp, selxlen);
     for (i = 0; i < selwlen; i++)
      {
       mask = (selxsp->ap[i] | ~selxsp->bp[i]) & (itemxsp->ap[i]
        | ~itemxsp->bp[i]);
/* SJM 01/08/99 - WRONG - for wide if == 0 always matches first */
       if ((((selxsp->ap[i] ^ itemxsp->ap[i])
        | (selxsp->bp[i] ^ itemxsp->bp[i])) & mask) != 0) goto nxt_x;
      }
     __pop_xstk();
     __pop_xstk();
     return(csip->csist);

nxt_x:
     __pop_xstk();
    }
  }
 __pop_xstk();
 if (dflt_csip->csist != NULL) return(dflt_csip->csist);
 return(NULL);
}

/*
 * execute the wait statement (not really a loop)
 *
 * if expression T, execute immediately
 * else block until change of variable in expr.
 * set up special net pin list elements (like events) until change
 * evaluate and remove if T
 *
 */
static int exec_wait(register struct st_t *stp)
{
 int tmp, rv;
 i_tev_ndx tevpi;
 struct xstk_t *xsp;
 struct delctrl_t *dctp;
 
 xsp = __eval_xpr(stp->st.swait.lpx);
 dctp = stp->st.swait.wait_dctp;
 if (xsp->xslen <= WBITS) tmp = ((xsp->ap[0] & ~xsp->bp[0]) != 0L);

 if (xsp->xslen <= WBITS)
  {
   if (stp->st.swait.lpx->is_real)
    {
     double d1;
 
     memcpy(&d1, xsp->ap, sizeof(double));
     tmp = (d1 != 0.0);
     /* must not emit z bit warning for real */
     /* LOOKATME - changing part of stack since really done with it */
     xsp->bp[0] = 0; 
    }
   else tmp = ((xsp->ap[0] & ~xsp->bp[0]) != 0L);
  }
 else tmp = (__cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen)) == 1);

 if (tmp == 1)
  {
   if (!vval_is0_(xsp->bp, xsp->xslen))
    __sgfinform(404, "TRUE wait expression contains some x/z bits");
   __pop_xstk();

   /* RELEASE remove
   if (__debug_flg)
    __dmp_dcemsg(dctp, "setting wait dce to nil");
   --- */

   /* disarm for this instance - wait now past */
   /* first time thru will be nils but faster to just assign */
   dctp->dceschd_tevs[__inum] = -1;
   __cur_thd->th_dctp = NULL;

   if (__st_tracing)
    { strcpy(__xs2, "--continuing"); rv = TRUE; goto tr_done; }  
   return(TRUE);
  }

 __pop_xstk();
 /* because of fast tev reclaim scheme - allocate and assign new */
 /* tev each time through here */
 /* notice this does not link on list */
 alloc_tev_(tevpi, TE_THRD, __inst_ptr, __simtime);
 __cur_thd->thdtevi = tevpi;
 __tevtab[tevpi].tu.tethrd = __cur_thd;
 /* if rexecuting task, algorithm is to cancel previous pending */
 /* delay control and emit warning */
 if (dctp->dceschd_tevs[__inum] != -1)
  {
   if (__cur_thd->assoc_tsk == NULL)
    {
     __sgfwarn(635,
      "INTERNAL BUG? - when reexecuting in %s cancel and rearm of wait",
      __msg2_blditree(__xs, __inst_ptr));
    }  
   else
    {
     __sgfwarn(635, "when reexecuting task %s cancel and rearm of wait",
     __msg_blditree(__xs, __inst_ptr, __cur_thd->assoc_tsk));
    }
  }
 /* RELEASE remove ---
 if (__debug_flg)
  __dmp_dcemsg(dctp, "setting wait dce to event");
 --- */
 dctp->dceschd_tevs[__inum] = tevpi;
 __cur_thd->th_dctp = dctp;
 if (__st_tracing) { strcpy(__xs2, "--suspend"); rv = FALSE; goto tr_done; }
 /* this arms this instances delay control in case expr. changes */
 return(FALSE);

tr_done:
 if (__st_tracing)
  {
   __tr_msg("trace: %-7d wait (%s) [cond: %d] %s\n",
    __slin_cnt, __msgexpr_tostr(__xs, stp->st.swait.lpx), tmp, __xs2);
  }
 return(rv);
}

/*
 * execute a for statement header
 * for is [init. assign; while (cond. exp) { <stmt>; <inc. assign stmt>; }
 * notice unlike C both initial statement and inc. statement must be assigns
 *
 * know inc. executed before here and never seen
 */
static int for_not_done(struct for_t *frs)
{
 int tmp, has_xzs;
 word val;
 double d1;
 struct xstk_t *xsp;

 /* must move and execute for inc. at end not beginning of loop */
 has_xzs = FALSE;
 xsp = __eval_xpr(frs->fortermx);
 if (xsp->xslen <= WBITS)
  {
   /* SJM 07/20/00 - must convert to real if real */
   if (frs->fortermx->is_real)
    { memcpy(&d1, xsp->ap, sizeof(double)); tmp = (d1 != 0.0); }
   else
    {
     val = xsp->bp[0];
     tmp = ((xsp->ap[0] & ~val) != 0L);
     if (val != 0) { if (tmp == 0) tmp = 3; else has_xzs = TRUE; }
    }
  }
 else
  {
   tmp = __cvt_lngbool(xsp->ap, xsp->bp, wlen_(xsp->xslen));
   if (tmp == 1) { if (!vval_is0_(xsp->bp, xsp->xslen)) has_xzs = TRUE; }
  }

 if (__st_tracing)
  {
   __cur_sofs = 0; 
   __dmp_forhdr((FILE *) NULL, frs);
   __trunc_exprline(TRTRUNCLEN, FALSE);
   __tr_msg("trace: %-7d %s) [cond: %d]\n", 
    __slin_cnt, __exprline, tmp);
   __cur_sofs = 0;
  }
 __pop_xstk();
 if (tmp == 1)
  {
   if (has_xzs)
    {
     __sgfinform(405, "for condition true but has some x/z bits");
    }
   return(TRUE);
  }
 /* notice any 1 implies true so will not get here */
 if (tmp == 3)
  {
   __sgfinform(406,
    "for loop terminated by FALSE expressions containing x/z bits");
  }
 /* done with loop */
 return(FALSE);
}

/* notice non label begin block optimized away by here */

/*
 * USER TASK/FUNCTION EXECUTION ROUTINES
 */

/*
 * build named block thread structure and then execute the block
 */
static void exec_namblk(struct st_t *stp)
{
 struct task_t *tskp;
 struct thread_t *thp;

 tskp = stp->st.snbtsk;
 /* indent block and statements within */
 if (__st_tracing)
  {
   if (tskp->tsktyp == FORK) strcpy(__xs, "fork");
   else strcpy(__xs, "begin");  
   __tr_msg("trace: %-7d %s : %s\n", __slin_cnt, __xs,
    tskp->tsksyp->synam);
  }
 /* use sub thread scheduling routine but just build and exec immediately */
 __cur_thd->thofscnt = 1;
 __cur_thd->thnxtstp = stp->stnxt;

 /* create normal thread structure but exec immediately - no schedule */
 thp = __setup_tsk_thread(tskp);

 thp->thpar = __cur_thd;
 __cur_thd->thofs = thp;
 /* move down but notice never an xmr instance loc. change */
 __cur_thd = thp;
 __cur_thd->th_itp = __inst_ptr; 
 /* DBG remove -- */
 if (__cur_thd->thnxtstp == NULL) __misc_terr(__FILE__, __LINE__);
 /* __dmp_tskthd(tskp, __inst_mod); */
 /* --- */
 /* always continue with down 1 thread - need thread only for possible */
 /* disable of named block */ 
}

/*
 * exec a task or named block as subthread of __cur_thd
 * stp is place to begin after completion
 * returns pointer to new sub thread 
 *
 * not used for fork-join because all fork join sub threads must be scheduled
 * caller must set current thread fields
 * will never see simple unnamed begin-end blocks here
 */
extern struct thread_t *__setup_tsk_thread(struct task_t *tskp)
{
 register struct thread_t *thp;
 register struct tskthrd_t *ttp;

 /* allocate a new thread */
 thp = __alloc_thrd();
 thp->thenbl_sfnam_ind = __sfnam_ind;
 thp->thenbl_slin_cnt = __slin_cnt;

 /* DBG remove -- */
 if (tskp == NULL) __arg_terr(__FILE__, __LINE__);
 /* --- */

 /* if task, set next stmt to first of task and link thread on tasks list */
 /* for disable, task list of all threads and thread to task link  */  
 /* schedule of task conflicts with thread task */
 /* DBG remove --- */
 if (thp->assoc_tsk != NULL && thp->assoc_tsk != tskp)
  __misc_sgfterr(__FILE__, __LINE__);
 /* --- */

 thp->thnxtstp = tskp->tskst;

 /* put on front of task thread lists */
 ttp = (struct tskthrd_t *) __my_malloc(sizeof(struct tskthrd_t)); 
 ttp->tthd_l = NULL;
 ttp->tthd_r = tskp->tthrds[__inum];
 tskp->tthrds[__inum] = ttp;
 if (ttp->tthd_r != NULL) ttp->tthd_r->tthd_l = ttp;
 ttp->tthrd = thp;
 thp->assoc_tsk = tskp;
 /* set the one list element that this thread connects to */
 thp->tthlst = ttp;
 return(thp);
}

/*
 * allocate threads and schedule execution of a fork-join 
 * know __cur_thd will be named block thread for label fork join - else
 * current thread
 *
 * no assoc. task since disabling fork-join label block disable 1 up normal
 * label block not fork join
 */
extern void __sched_fork(register struct st_t *stp)
{
 register int fji;
 register struct thread_t *thp;
 int sav_slin_cnt, sav_sfnam_ind;
 struct thread_t *last_thp;
 struct st_t *fjstp;

 /* DBG remove */
 if (__cur_thd->thofscnt != 0) __misc_terr(__FILE__, __LINE__); 
 /* --- */ 
 /* convert current thread (one-up) to fork joint header */
 /* and build (link in) list of per statement threads */  
 last_thp = NULL;
 for (fji = 0;; fji++)
  {
   if ((fjstp = stp->st.fj.fjstps[fji]) == NULL) break;
   
   /* SJM 03/07/02 - for optimizer must always schedule 1st stmt of unblk */
   /* instead of unnamed blk as now */
   if (fjstp->stmttyp == S_UNBLK) fjstp = fjstp->st.sbsts;
   
   /* using location of fork-join statement as enable loc. not fork loc. */
   sav_sfnam_ind = __sfnam_ind;
   sav_slin_cnt = __slin_cnt;
   __sfnam_ind = fjstp->stfnam_ind;
   __slin_cnt = fjstp->stlin_cnt;
   /* schedule each subthread after building it */
   thp = sched_fj_subthread(fjstp);

   __sfnam_ind = sav_sfnam_ind;
   __slin_cnt = sav_slin_cnt;

   __cur_thd->thofscnt += 1;
   if (last_thp == NULL) __cur_thd->thofs = thp;
   else { thp->thleft = last_thp; last_thp->thright = thp; }
   thp->thpar = __cur_thd;
   thp->th_itp = __inst_ptr;
   /* flag on fork-join component to indicate must look for assoc tsk up */
   thp->th_fj = TRUE;
   last_thp = thp;
  }
}

/*
 * setup and schedule execution of one fork-join subthread of __cur_thd
 *
 * stp is place to begin when event processed
 * returns pointer to thread value in scheduled event
 * caller must set current thread fields
 */
static struct thread_t *sched_fj_subthread(struct st_t *stp)
{
 register struct thread_t *thp;
 i_tev_ndx tevpi;

 /* allocate a new thread */
 thp = __alloc_thrd();
 thp->thenbl_sfnam_ind = __sfnam_ind;
 thp->thenbl_slin_cnt = __slin_cnt;

 /* set the one fj statement (or list) as next stmt of subthread */
 thp->thnxtstp = stp;
 
 /* allocate an event for this fork-join component statement */
 /* at end of current time slot */
 alloc_tev_(tevpi, TE_THRD, __inst_ptr, __simtime);

 /* link thread back to event */
 thp->thdtevi = tevpi;
 __tevtab[tevpi].tu.tethrd = thp;

 if (__debug_flg && __st_tracing)
  {
   __tr_msg("trace: %-7d -- schedule new subthread at %s continue at %s\n",
    __slin_cnt, __bld_lineloc(__xs, thp->thenbl_sfnam_ind,
    thp->thenbl_slin_cnt), __bld_lineloc(__xs2, stp->stfnam_ind,
    stp->stlin_cnt));
  }

 /* this must go on front because interactive statement must complete */
 __add_ev_to_front(tevpi);
 return(thp);
}

/*
 * add an event to front of current queue
 * for various procedural control - must go front so interactive completes
 * before any other events processed
 *
 * scheme is to always execute procedural as soon as possible
 * but declarative as late as possible
 */
extern void __add_ev_to_front(register i_tev_ndx tevpi)
{
 if (!__processing_pnd0s)
  {
   /* adding to front is just after current since now processing current */
   if (__cur_tevpi == -1)
    {
     if (__cur_te_hdri == -1) __cur_te_hdri = __cur_te_endi = tevpi; 
     else { __tevtab[tevpi].tenxti = __cur_te_hdri; __cur_te_hdri = tevpi; }
    }
   else
    {
     if (__cur_tevpi != __cur_te_endi)
      __tevtab[tevpi].tenxti = __tevtab[__cur_tevpi].tenxti;
     else __cur_te_endi = tevpi;
     __tevtab[__cur_tevpi].tenxti = tevpi;
    }
   __num_twhevents++;
   /* need to make sure number of timing wheel events matches cur number */
   __twheel[__cur_twi]->num_events += 1;
  }
 else
  {
   /* also in pnd 0's front is just after current if set */
   if (__cur_tevpi == -1)
    {
     /* notice during add net chg elements cur tevp nil, so common to add */
     /* to end */ 
     if (__p0_te_hdri == -1) __p0_te_hdri = __p0_te_endi = tevpi; 
     else { __tevtab[tevpi].tenxti = __p0_te_hdri; __p0_te_hdri = tevpi; }
    }
   else
    {
     if (__cur_tevpi != __p0_te_endi)
      __tevtab[tevpi].tenxti = __tevtab[__cur_tevpi].tenxti;
     else __p0_te_endi = tevpi;
     __tevtab[__cur_tevpi].tenxti = tevpi;
    }
   /* this does not go on timing wheel or get counted */
  }
}

/*
 * allocate a new thread
 */
extern struct thread_t *__alloc_thrd(void)
{
 register struct thread_t *thp;

 thp = (struct thread_t *) __my_malloc(sizeof(struct thread_t));
 init_thrd(thp);
 return(thp);
}

/*
 * initialize a new thread
 */
static void init_thrd(register struct thread_t *thp)
{
 thp->tsk_stouts = FALSE;
 thp->th_dsable = FALSE;
 thp->th_rhsform = FALSE;
 thp->th_fj = FALSE;
 thp->th_ialw = FALSE;
 thp->th_postamble = FALSE;
 /* off-spring count is 0 unless incremented when sub thread created */  
 thp->thofscnt = 0;
 thp->thnxtstp = NULL;
 thp->thpar = thp->thright = thp->thleft = thp->thofs = NULL;
 thp->tthlst = NULL;
 thp->assoc_tsk = NULL;
 thp->th_dctp = NULL;
 thp->thdtevi = -1;
 thp->thenbl_sfnam_ind = 0;
 thp->thenbl_slin_cnt = 0;
 thp->th_rhswp = NULL;
 thp->th_rhswlen = -1;
 thp->th_itp = NULL;
 thp->th_hctrl = NULL;
}

/*
 * execute a task call
 *
 * thread suspend mechanism set up but works by continuing with first
 * stmt in tsk body (can be execed as iop) - thread mechanism fixed 
 * up so suspend works right including handling of disable and tsk outs
 *
 * know for user tasks, argument list exactly matches definition list 
 * can improve this by preprocessing call/return evaluation
 */
extern struct st_t *__exec_tskcall(struct st_t *stp)
{
 register struct expr_t *xp;
 register struct task_pin_t *tpp;
 int argi;
 struct tskcall_t *tkcp;
 struct expr_t *tkxp, *rhsxp;
 struct sy_t *syp;
 struct task_t *tskp;
 struct xstk_t *xsp;
 struct net_t *np;
 struct thread_t *thp;
 struct itree_t *tsk_itp;
 struct st_t *stp2;

 tkcp = &(stp->st.stkc);
 tkxp = tkcp->tsksyx;
 syp = tkxp->lu.sy;

 if (syp->sytyp == SYM_STSK)
  {
   /* no time movement in system tasks */
   /* return NULL, to suspend thread - non null to continue as to next st */
   /* this does own tracing */
   if (__st_tracing)
    {
     __cur_sofs = 0;
     __dmp_tskcall((FILE *) NULL, stp);
     __trunc_exprline(TRTRUNCLEN, FALSE);
     __tr_msg("trace: %-7d %s\n", __slin_cnt,
       __exprline);
     __cur_sofs = 0;
    }
   return(__exec_stsk(stp, syp, tkcp));
  }
 tskp = syp->el.etskp;
 tpp = tskp->tskpins;

 if (tkxp->optyp == GLBREF)
  {
   /* must get tsk exec itree location but cannot change to yet */
   __xmrpush_refgrp_to_targ(tkxp->ru.grp);
   tsk_itp = __inst_ptr;
   /* notice need to print new location for xmr task */
   if (__st_tracing) tr_resume_msg();
   __pop_itstk();
  }
 else tsk_itp = NULL;
 if (__st_tracing)
  {
   __cur_sofs = 0;
   __adds("<** enabling task ");
   __adds(__to_idnam(tkxp));
   addch_('(');
  }

 /* must assign to task variables since values persist */
 /* user tasks are value-result */
 argi = 0;
 for (xp = tkcp->targs; xp != NULL; xp = xp->ru.x, tpp = tpp->tpnxt, argi++)
  {
   if (tpp->trtyp != IO_OUT)
    {
     /* assign rhs in or inout arg. expr. to task local variable */
     np = tpp->tpsy->el.enp;
     rhsxp = xp->lu.x;
     xsp = __eval_xpr(rhsxp);
     eval_tskassign_rhsexpr(xsp, (np->ntyp == N_REAL), np->nwid,
      (rhsxp->is_real == 1), (rhsxp->has_sign == 1));
     if (__st_tracing) tradd_tf_argval(argi, np, xsp);
     /* if xmr call, afer eval in cur. itree loc. must store in xmr dest */
     if (tsk_itp != NULL)
      {
       __push_itstk(tsk_itp);
       __chg_st_val(np, xsp->ap, xsp->bp);
       __pop_itstk();
      }
     else __chg_st_val(np, xsp->ap, xsp->bp);
     __pop_xstk();
    }
   else if (__st_tracing)
    {
     /* for tracing output value on entry, need caller's itree loc. */
     np = tpp->tpsy->el.enp;
     push_xstk_(xsp, np->nwid);
     __ld_wire_val(xsp->ap, xsp->bp, np);
     tradd_tf_argval(argi, np, xsp);
     __pop_xstk();
    }
  }
 if (__st_tracing)
  {
   __adds(")"); 
   __trunc_exprline(TRTRUNCLEN, FALSE);
   __tr_msg("trace: %-7d %s\n", __slin_cnt, __exprline);
   __cur_sofs = 0;
  }

 /* use sub thread scheduling routine but just build and exec immediately */
 __cur_thd->thofscnt = 1;

 /* if xmr task call replace top of instance stack here - cur_thd has up 1 */
 if (tsk_itp != NULL) { __pop_itstk(); __push_itstk(tsk_itp); }

 /* trick here is that must not advance statement since need to store */
 thp = __setup_tsk_thread(tskp);

 if (tskp->thas_outs || __st_tracing)
  {
   thp->tsk_stouts = TRUE;
   /* must set thrd nxt stmt to the task call so can find task after */ 
   /* task completed suspend so can find tsk to set out params in */
   /* fixup to skip of non loop end gotos after tsk outs stored */
   __cur_thd->thnxtstp = stp;
   __cur_thd->th_postamble = TRUE;
  }
 /* SJM 04/05/02 - skip over all non loop end gotos so can exec actual stmt */
 else
  {
   stp2 = stp->stnxt;
   if (stp2 == NULL) __cur_thd->thnxtstp = NULL;
   else if (stp2->stmttyp != S_GOTO) __cur_thd->thnxtstp = stp2;
   else if (stp2->lpend_goto) __cur_thd->thnxtstp = stp2;
   else
    {
     for (;;)
      {
       /* know on entry stp2 goto */
       stp2 = stp2->st.sgoto;
       if (stp2 == NULL || stp2->stmttyp != S_GOTO)
        { __cur_thd->thnxtstp = stp2; break; }
       if (stp2->lpend_goto) { __cur_thd->thnxtstp = stp2; break; }
      }
    }
  }

 thp->thpar = __cur_thd;
 __cur_thd->thofs = thp;
 /* make task thread current */
 __cur_thd = thp;
 __cur_thd->th_itp = __inst_ptr;
 /* DBG remove ---
 __dmp_tskthd(tskp, __inst_mod);
 --- */
 return(thp->thnxtstp);
}

/*
 * print a task or function argument value    
 */
static void tradd_tf_argval(int argi, struct net_t *np, struct xstk_t *xsp)
{
 char s1[RECLEN];
 int signv, base;

 if (argi != 0) __adds(", "); 
 if (!vval_is0_(xsp->bp, xsp->xslen) && np->ntyp != N_REAL)
  {
   sprintf(s1, "%d'h", xsp->xslen); 
   __adds(s1);
   __regab_tostr(s1, xsp->ap, xsp->bp, xsp->xslen, BHEX, FALSE);
  }
 else
  {
   signv = FALSE; base = BHEX;
   if (np->ntyp == N_REAL) base = BDBLE; 
   else if (np->n_signed) { base = BDEC; signv = TRUE; } 
   __regab_tostr(s1, xsp->ap, xsp->bp, xsp->xslen, base, signv);
  }
 __adds(s1);
}

/*
 * store task return output parameters
 *
 * if disabled never get here
 * tricky because must eval using tos itree loc. but assign to 1 under
 * also get here even if no out args but statement tracing on
 *
 * this is called from task thread and itree loc. but assigns to one up
 * thread and itree location
 */
static void store_tskcall_outs(struct st_t *tskcall_stp) 
{
 register struct expr_t *xp;
 register struct task_pin_t *tpp;
 int first_time, base, signv;
 struct tskcall_t *tkcp;
 struct expr_t *tkxp, *lhsxp;
 struct task_t *tskp;
 struct xstk_t *xsp;
 struct net_t *np;
 char s1[RECLEN];

 tkcp = &(tskcall_stp->st.stkc);
 tkxp = tkcp->tsksyx;

 tskp = tkxp->lu.sy->el.etskp;
 if (__st_tracing)
  {
   __cur_sofs = 0;
   __adds("**> returning from task ");
   __adds(__to_idnam(tkxp));
   addch_('(');
  }
 tpp = tskp->tskpins;
 first_time = TRUE;
 for (xp = tkcp->targs; xp != NULL; xp = xp->ru.x, tpp = tpp->tpnxt)
  {
   if (tpp->trtyp == IO_IN) continue;

   /* assign task local param var. value to lhs call argument */
   /* xp->lu.x is rhs src., np is lhs dest. var. */
   np = tpp->tpsy->el.enp;
   push_xstk_(xsp, np->nwid);
   /* need load value here because, need to decode storage rep */
   __ld_wire_val(xsp->ap, xsp->bp, np);
   lhsxp = xp->lu.x;
   eval_tskassign_rhsexpr(xsp, (lhsxp->is_real == 1), lhsxp->szu.xclen,
    (np->ntyp == N_REAL), (np->n_signed == 1));
   /* np here is rhs */
   if (__st_tracing) 
    {
     if (first_time) first_time = FALSE; else __adds(", "); 
     if (np->ntyp != N_REAL && !vval_is0_(xsp->bp, xsp->xslen))
      {
       sprintf(s1, "%d'h", xsp->xslen); 
       __adds(s1);
       __regab_tostr(s1, xsp->ap, xsp->bp, xsp->xslen, BHEX, FALSE);
      }
     else
      {
       signv = FALSE; base = BHEX;
       if (np->ntyp == N_REAL) base = BDBLE; 
       else if (np->n_signed) { base = BDEC; signv = TRUE; } 
       __regab_tostr(s1, xsp->ap, xsp->bp, xsp->xslen, base, signv); 
      }
     __adds(s1);
    } 
   /* notice for xmr task enable, must eval in task itree place */
   /* but store top of expr. stack in calling itree place */
   if (tkxp->optyp == GLBREF)
    {
     __push_itstk(__cur_thd->thpar->th_itp);
     __exec2_proc_assign(lhsxp, xsp->ap, xsp->bp);
     __pop_itstk();
    }
   else __exec2_proc_assign(lhsxp, xsp->ap, xsp->bp);
   __pop_xstk();
  }
 if (__st_tracing)
  {
   __adds(")"); 
   __trunc_exprline(TRTRUNCLEN, FALSE);
   __tr_msg("trace: %-7d %s\n", __slin_cnt, __exprline);
   __cur_sofs = 0;
  }
}

/*
 * execute a user function call operator in an expression
 * ndp is FCALL expression node - ru is operand list
 * user functions only take input args 
 * notice local variables presist and puts return value on top of expr stk 
 */
extern void __exec_func(struct sy_t *fsyp, register struct expr_t *ndp) 
{
 register struct expr_t *argxp;
 register struct task_pin_t *tpp;
 int savslin_cnt, savsfnam_ind, nd_thdfree;
 struct itree_t *func_itp, *xmr_savitp;
 struct st_t *stp;
 struct task_t *tskp;
 struct tev_t *tevp;
 int argi;
 struct gref_t *grp;
 struct xstk_t *xsp;
 struct net_t *np;
 struct expr_t *rhsxp;
 
 /* for decl. rhs, maybe no thrd - bld for 1st call else take over cur. */ 
 nd_thdfree = FALSE;
 if (__cur_thd == NULL)
  {
   __cur_thd = __alloc_thrd();
   __cur_thd->th_itp = __inst_ptr;
   nd_thdfree = TRUE;
  }
 /* DBG remove --- */
 else if (__cur_thd->th_itp != __inst_ptr) __misc_terr(__FILE__, __LINE__);
 /* --- */ 

 /* function source will be dump later */ 
 if (__st_tracing)
  {
   __cur_sofs = 0;
   __adds("<** calling function ");
   __adds(__to_idnam(ndp->lu.x));
   addch_('(');
  }

 xmr_savitp = __inst_ptr;
 /* function call prep. block */

 argi = 0;
 tskp = fsyp->el.etskp;
 tpp = tskp->tskpins->tpnxt;
 /* if global, local variables accessed from target (defining mod) inst */
 if (ndp->lu.x->optyp == GLBREF)
  {
   grp = ndp->lu.x->ru.grp;
   __xmrpush_refgrp_to_targ(grp);
   if (__st_tracing) tr_resume_msg();
   func_itp = __inst_ptr;
   /* cannot change to func xmr place yet */
   __pop_itstk();
  }
 else func_itp = NULL;

 /* evaluate and store input params */
 /* 1st tpp is by convention is return value but 1st arg is real arg */
 /* know number matches exactly (no ,,) or will not get here */ 
 argi = 0;
 for (argxp = ndp->ru.x; argxp != NULL; argxp = argxp->ru.x,
  tpp = tpp->tpnxt, argi++)
  {
   /* if xmr call, must eval. these in current not func. */
   /* assign rhs in or inout arg. expr. to task local variable */
   np = tpp->tpsy->el.enp;
   rhsxp = argxp->lu.x;
   xsp = __eval2_xpr(rhsxp);
   eval_tskassign_rhsexpr(xsp, (np->ntyp == N_REAL), np->nwid,
    (rhsxp->is_real == 1), (rhsxp->has_sign == 1));

   if (__st_tracing) tradd_tf_argval(argi, np, xsp);

   /* notice can cause propagate reg xmr on rhs that is function arg */
   /* if xmr need to store in down itree location */
   if (func_itp != NULL)
    {
     __push_itstk(func_itp);
     __chg_st_val(np, xsp->ap, xsp->bp);
     __pop_itstk();
    }
   else __chg_st_val(np, xsp->ap, xsp->bp);
   __pop_xstk();
  }

 if (__st_tracing)
  {
   __adds(")"); 
   __trunc_exprline(TRTRUNCLEN, FALSE);
   __tr_msg("trace: %-7d %s\n", __slin_cnt, __exprline);
   __cur_sofs = 0;
  }
 /* this is dynamic call list */
 if (++__fcspi >= __maxfcnest) grow_fcstk();
 __fcstk[__fcspi] = tskp;
 savslin_cnt = __slin_cnt;
 savsfnam_ind = __sfnam_ind;
 /* if xmr function call replace top - relative xmr's not though itstk */ 
 if (func_itp != NULL)
  { __pop_itstk(); __push_itstk(func_itp); __cur_thd->th_itp = __inst_ptr; }

 /* cannot schedule and resume inside func. so suspend and schedule */
 /* then unsuspend and cancel event */
 stp = tskp->tskst;
 __cur_thd->thnxtstp = stp;
 /* if stepping, make sure first execed */
 if (__single_step) __step_from_thread = FALSE;

again:
 __stmt_suspend = FALSE;
 /* step returns after 1 statement (to new line) or end of func */
 if (__single_step && __cur_thd->th_hctrl == NULL) step_exec_stmt(stp);
 else if (__st_tracing || __single_step)
  {
    brktr_exec_stmts(stp);
  }
 else exec_stmts(stp);

 /* happens if hit break or step or ^c hit - suspend routine just execed */
 if (__stmt_suspend)
  {
   __do_interactive_loop();
   /* tricky code for func - restart and cancel scheduled resume event */
   /* DBG remove --- */  
   if (__fsusp_tevpi == -1) __misc_terr(__FILE__, __LINE__);

   tevp = &(__tevtab[__fsusp_tevpi]);
   if (__inst_ptr != tevp->teitp || __inst_ptr != __suspended_itp) 
    __misc_terr(__FILE__, __LINE__);
   /* --- */
   /* restore from func. suspended event and cancel event */
   __cur_thd = tevp->tu.tethrd;
   tevp->te_cancel = TRUE;
   __fsusp_tevpi = -1L;

   /* undo suspend */
   __cur_thd->thdtevi = -1;
   __suspended_thd = NULL;
   __suspended_itp = NULL;
   stp = __cur_thd->thnxtstp;
   goto again;
  }
 
 __slin_cnt = savslin_cnt;
 __sfnam_ind = savsfnam_ind;
 __fcspi--;

 /* SJM 05/12/03 - do not need block here */
 /* return block */
 /* put assign func return variable (func. name) value on tos */
 /* key here is task name local variable has declaration from func hdr */
 /* notice ok if not assigned, to will just return x */
 np = tskp->tskpins->tpsy->el.enp;
 push_xstk_(xsp, np->nwid);
 /* caller must intepret type of value on tos */
 /* for itp this must be loaded from dest. */
 __ld_wire_val(xsp->ap, xsp->bp, np);
 /* if xmr replace top with original and put back thread itp */ 
 if (func_itp != NULL)
 { __pop_itstk(); __push_itstk(xmr_savitp); __cur_thd->th_itp = __inst_ptr; }

 if (__st_tracing)
  {
   int signv, base;

   if (np->ntyp != N_REAL && !vval_is0_(xsp->bp, xsp->xslen))
    {
     sprintf(__xs2, "%d'h%s", xsp->xslen, __regab_tostr(__xs, xsp->ap,
      xsp->bp, xsp->xslen, BHEX, FALSE));
    }
   else 
    {
     signv = FALSE; base = BHEX;
     if (np->ntyp == N_REAL) base = BDBLE; 
     else if (np->n_signed) { base = BDEC; signv = TRUE; } 
     __regab_tostr(__xs2, xsp->ap, xsp->bp, xsp->xslen, base, signv);
    }
   __tr_msg("trace: %-7d **> [%s] returned by function %s\n",
    __slin_cnt, __xs2, __to_idnam(ndp->lu.x));
  }

 if (nd_thdfree)
  {
   __my_free((char *) __cur_thd, sizeof(struct thread_t));
   __cur_thd = NULL;
  } 
 return;
}

/*
 * routine to grow fcstk (function call no display local variables)
 */
static void grow_fcstk(void)
{
 register int i;
 int old_maxnest;
 int osize, nsize;

 old_maxnest = __maxfcnest;
 osize = old_maxnest*sizeof(struct task_t *);
 /* grow by 50% after certain point */
 if (__maxfcnest >= 2000) __maxfcnest += __maxfcnest/2;
 else __maxfcnest *= 2;
 nsize = __maxfcnest*sizeof(struct task_t *);
 __fcstk = (struct task_t **) __my_realloc((char *) __fcstk, osize, nsize);
 for (i = old_maxnest; i < __maxfcnest; i++) __fcstk[i] = NULL;
 if (__debug_flg)
  __dbg_msg("+++ fcall stack grew from %d bytes to %d\n", osize, nsize); 
}

/*
 * execute a system function call operator
 * ndp is actual FCALL node
 * leaves return value on top of expr. stack but does not return it
 */
extern void __exec_sysfunc(register struct sy_t *fsyp,
 register struct expr_t *ndp) 
{
 register struct xstk_t *xsp, *xsp2;
 register struct expr_t *fax;
 int ival;
 int 
 unsigned uval;
 word64 timval;
 double d1;
 struct sysfunc_t *sfbp;

 sfbp = fsyp->el.esyftbp;
 switch (sfbp->syfnum) {
  /* SJM 09/08/03 - old fopen just for mcds */
  case STN_FOPEN:
   fax = ndp->ru.x->lu.x;
   uval = mc_do_fopen(fax);
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) uval;  
   xsp->bp[0] = 0L;
   break; 
  case STN_STIME: 
  case STN_TIME:
   /* convert ticks to user time (maybe smaller) and return WBIT form */
   /* with warn if does not fit */
   /* this can be 0 - know conversion to user time always succeeds */
   if (!__inst_mod->mno_unitcnv)
    __cnv_ticks_tonum64(&timval, __simtime, __inst_mod);
   else timval = __simtime;
   if (sfbp->syfnum == STN_STIME)
    { 
     push_xstk_(xsp, WBITS);
     if (timval > WORDMASK_ULL)
      {
       __sgfinform(411, "system function %s result does not fit in %d bits",
        fsyp->synam, WBITS);
      }
     xsp->ap[0] = (word) (timval & WORDMASK_ULL);
     xsp->bp[0] = 0L;
    }
   else
    {
     push_xstk_(xsp, TIMEBITS);
     xsp->ap[0] = (word) (timval & WORDMASK_ULL);
     xsp->ap[1] = (word) ((timval >> 32) & WORDMASK_ULL);
     xsp->bp[0] = xsp->bp[1] = 0L;
    }
   break;
  case STN_REALTIME:
   /* for time as user world (unscaled) time, must convert to real first */
   d1 =__unscale_realticks(&__simtime, __inst_mod);
   push_xstk_(xsp, WBITS);
   /* copy from 1st to 2nd */
   memcpy(xsp->ap, &d1, sizeof(double));
   break;
  case STN_STICKSTIME:
   push_xstk_(xsp, WBITS);
   if (__simtime > WORDMASK_ULL)
    {
     __sgfinform(411, "system function %s result does not fit in %d bits",
      fsyp->synam, WBITS);
    }
   xsp->ap[0] = (word) (__simtime & WORDMASK_ULL);
   xsp->bp[0] = 0L;
   break;
  case STN_TICKSTIME:
   push_xstk_(xsp, TIMEBITS);
   xsp->ap[0] = (word) (__simtime & WORDMASK_ULL);
   xsp->ap[1] = (word) ((__simtime >> 32) & WORDMASK_ULL);
   xsp->bp[0] = xsp->bp[1] = 0L;
   break;
  case STN_BITSTOREAL:
   /* this converts the a parts of a 64 bit reg to a wbit real */ 
   /* know this will be 64 bit or previous error */
   fax = ndp->ru.x->lu.x;
   xsp = __eval_xpr(fax);
   if (xsp->xslen != 64)
    {
     __sgfwarn(636, "$bitstoreal of %s value not 64 bits - set to 0",
      __regab_tostr(__xs, xsp->ap, xsp->bp, xsp->xslen, BHEX, FALSE));
conv_0:
     d1 = 0.0;
     memcpy(xsp->ap, &d1, sizeof(double));
     /* SJM 07/05/03 - need to also adjust b part for real */
     xsp->xslen = WBITS;
     xsp->bp = &(xsp->ap[1]);
     break;
    } 

   /* notice must silently convert x to 0.0, since port will start at x */
   if (!vval_is0_(xsp->bp, xsp->xslen)) goto conv_0;
   /* finally, convert to real - assuming bits good - should convert to */
   /* something and see what error code is set ? */
   /* this is stupid but allow looking at the number in debugger */  
   memcpy(&d1, xsp->ap, sizeof(double)); 
   /* DBG - LOOKATME - why here 
   if (finite(d1) == 0) __arg_terr(__FILE__, __LINE__);
   -- */
   memcpy(xsp->ap, &d1, sizeof(double)); 

   xsp->bp = &(xsp->ap[1]);
   xsp->xslen = WBITS;
   break;
  case STN_REALTOBITS:
   push_xstk_(xsp, 64);
   fax = ndp->ru.x->lu.x;
   xsp2 = __eval_xpr(fax);
   /* notice double stored with b (x/z) part as WBITS really 64 */
   xsp->ap[0] = xsp2->ap[0];
   xsp->ap[1] = xsp2->bp[0];
   xsp->bp[0] = xsp->bp[1] = 0L;
   __pop_xstk();
   break;
  case STN_ITOR:
   /* know arg must be 32 or narrower */
   fax = ndp->ru.x->lu.x;
   xsp = __eval_xpr(fax);
   if (xsp->bp[0] != 0L)
    {
     __sgfwarn(631,
      "system function %s argument %s x/z value converted to 0.0", 
       fsyp->synam, __msgexpr_tostr(__xs, fax));
     d1 = 0.0;
    }
   else
    {
     if (fax->has_sign) { ival = (int) xsp->ap[0]; d1 = (double) ival; } 
     else d1 = (double) xsp->ap[0];
    }
   /* notice reusing xsp since know size of both is WBITS */
   memcpy(xsp->ap, &d1, sizeof(double));
   break;
  case STN_RTOI:
   /* think semantics is to convert keeping sign - number maybe 2's compl */
   fax = ndp->ru.x->lu.x;
   xsp = __eval_xpr(fax);
   memcpy(&d1, xsp->ap, sizeof(double));
   /* DBG - LOOKATME - why here 
   if (finite(d1) == 0) __arg_terr(__FILE__, __LINE__);
   -- */
   ival = (int) d1;
   /* reuse expr. that know is WBITS */ 
   xsp->bp[0] = 0L;
   xsp->ap[0] = (word) ival;
   break;
  case STN_RANDOM:
   __exec_sfrand(ndp);
   break; 
  case STN_COUNT_DRIVERS:
   exec_count_drivers(ndp);
   break;
  case STN_DIST_UNIFORM:
   __exec_dist_uniform(ndp);
   break;
  case STN_DIST_EXPONENTIAL:
   __exec_dist_exp(ndp);
   break;
  case STN_DIST_NORMAL:
   __exec_dist_stdnorm(ndp);
   break;
  case STN_DIST_CHI_SQUARE:
   __exec_chi_square(ndp);
   break;
  case STN_DIST_POISSON:
   __exec_dist_poisson(ndp);
   break;
  case STN_DIST_T:
   __exec_dist_t(ndp);
   break;

  case STN_DIST_ERLANG:
   /* not yet implemented make 32 bit x */
   __sgfwarn(550, "system function %s not implemented - returning 32'bx",
    fsyp->synam);
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = ALL1W;
   xsp->bp[0] = ALL1W;
   break;
  case STN_Q_FULL:
   exec_qfull(ndp);  
   break;
  case STN_SCALE:
   fax = ndp->ru.x->lu.x;
   /* DBG remove */
   if (fax->optyp != GLBREF) __arg_terr(__FILE__, __LINE__);
   /* --- */
   /* this puts scale value on top of stack */
   __exec_scale(fax);
   break;
  case STN_TESTPLUSARGS:
   exec_testplusargs(ndp);      
   break;
  case STN_SCANPLUSARGS:
   exec_scanplusargs(ndp);      
   break;
  case STN_RESET_COUNT:
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) __reset_count;
   xsp->bp[0] = 0L;
   break;
  case STN_RESET_VALUE:
   push_xstk_(xsp, WBITS);
   /* value may be int (signed) - caller will interpret */
   xsp->ap[0] = (word) __reset_value;
   xsp->bp[0] = 0L;
   break;
  case STN_GETPATTERN:
   /* should never see get pattern here */
   __arg_terr(__FILE__, __LINE__);
   break;
  case STN_COS: case STN_SIN: case STN_TAN:
  case STN_ACOS: case STN_ASIN: case STN_ATAN:
  case STN_COSH: case STN_SINH: case STN_TANH:
  case STN_ACOSH: case STN_ASINH: case STN_ATANH:
  case STN_LN: case STN_LOG10: case STN_ABS: case STN_SQRT: case STN_EXP:
  case STN_HSQRT: case STN_HLOG: case STN_HLOG10: case STN_HDB: 
   fax = ndp->ru.x->lu.x;
   exec_1arg_transcendental(sfbp->syfnum, fax);
   break;
  case STN_INT:
   exec_transcendental_int(ndp);
   break;
  case STN_SGN:
   exec_transcendental_sign(ndp);
   break;
  case STN_POW: case STN_HPOW: case STN_HPWR: case STN_HSIGN:
   exec_transcendental_powsign(sfbp->syfnum, ndp);
   break;
  case STN_MIN: case STN_MAX:
   exec_transcendental_minmax(sfbp->syfnum, ndp);
   break;
  case STN_ATAN2:
   exec_transcendental_atan2(ndp);
   break;
  case STN_HYPOT:
   exec_transcendental_hypot(ndp);
   break;
  default:
   /* DBG remove --- */
   if (sfbp->syfnum < BASE_VERIUSERTFS || (int) sfbp->syfnum > __last_systf)
    __case_terr(__FILE__, __LINE__);
   /* --- */
   /* call pli system function calltf here - leave ret. value on stk */ 
   if (sfbp->syfnum <= __last_veriusertf) __pli_func_calltf(ndp);
   /* vpi_ systf after veriusertfs up to last systf */
   else __vpi_sysf_calltf(ndp);
 } 
}

/*
 * execute the count driver system function
 * notice these leave return (count) on expr. stack
 * change so count forced when force implemented
 */
static void exec_count_drivers(struct expr_t *ndp)
{
 register struct net_pin_t *npp;
 register struct expr_t *axp;
 register int i;
 int ri1, ri2, nd_itpop, biti, indi, is_forced, numdrvs, drvcnt[4];
 unsigned val;
 byte *sbp;
 struct net_t *np;
 struct xstk_t *xsp;
 struct expr_t *idndp;
 struct gref_t *grp;

 nd_itpop = FALSE;
 for (i = 0; i < 4; i++) drvcnt[i] = 0; 
 is_forced = 0;
 numdrvs = 0;
 biti = -1;
 /* first get 1st argument value */
 axp = ndp->ru.x->lu.x; 
 /* know if bit select will be non x or earlier error to stop execution */
 if (axp->optyp == LSB) 
  {
   /* must eval. index expr. in ref. not target in case xmr */ 
   xsp = __eval_xpr(axp->ru.x);
   biti = xsp->ap[0];
   __pop_xstk(); 
   idndp = axp->lu.x;   
  }
 else idndp = axp;

 if (idndp->optyp == GLBREF)
  {
   grp = idndp->ru.grp;
   __xmrpush_refgrp_to_targ(grp);
   nd_itpop = TRUE;
  }
 np = idndp->lu.sy->el.enp;

 if (biti == -1) indi = 0; else indi = biti;
 if (np->frc_assgn_allocated)
  {
   if (np->ntyp >= NONWIRE_ST)
    {
     if (np->nu2.qcval[2*__inum].qc_active ||
      np->nu2.qcval[2*__inum + 1].qc_active) is_forced = TRUE;
     }
   else
    {
     if (np->nu2.qcval[np->nwid*__inum + indi].qc_active)
      is_forced = TRUE;
    }
  }

 /* since just evaluating read only must evaluate all drivers in here */
 /* inout itp and inout mpp NULL which is needed */ 
 for (npp = np->ndrvs; npp != NULL; npp = npp->npnxt)
  {
   if (npp->npproctyp == NP_PROC_FILT
    && npp->npaux->npu.filtitp != __inst_ptr) continue;

   /* need to handle -2 IS specific bit select */ 
   __get_bidnpp_sect(np, npp, &ri1, &ri2);
   if (npp->npaux == NULL || ri1 == -1 || biti == -1) goto got_match;
   if (biti > ri1 || biti < ri2) continue;

got_match:
   /* need to make sure driver is loaded - no concept of changing here */
   switch (npp->npntyp) {
    case NP_VPIPUTV:
     /* for added vpi driver - this inst. or bit may not be added */
     /* if not added (used), do not count */
     if (!__has_vpi_driver(np, npp)) continue; 
     goto load_driver;

    case NP_GATE: case NP_CONTA: case NP_MDPRT: case NP_PB_MDPRT:
    case NP_ICONN: case NP_TFRWARG:
load_driver:
     /* load driver leaves value on expr. stack */
     if (np->n_stren)
      { 
       if ((xsp = __ld_stwire_driver(npp)) == NULL) break;
       sbp = (byte *) xsp->ap;
       val = sbp[indi] & 3;
       __pop_xstk();
      }
     else
      {
       xsp = __ld_wire_driver(npp);
       if (biti == -1) val = (xsp->ap[0] & 1L) | ((xsp->bp[0] & 1L) << 1); 
       else val = (rhsbsel_(xsp->ap, biti)) | (rhsbsel_(xsp->bp, biti) << 1);
       __pop_xstk();
      }
     (drvcnt[val])++;
     break;
    case NP_PULL: break; 
    default: __case_terr(__FILE__, __LINE__);
   } 
   /* other drivers such as pull just ingored in determing drivers */
  }
 if (nd_itpop) __pop_itstk();

 /* finally do the storing */
 numdrvs = drvcnt[0] + drvcnt[1] + drvcnt[3];
 /* know at least one argument */
 push_xstk_(xsp, WBITS);
 xsp->ap[0] = 0;
 xsp->bp[0] = 0;
 if ((axp = ndp->ru.x->ru.x) == NULL) goto just_ret; 
 for (i = 1; axp != NULL; axp = axp->ru.x, i++)
  {
   if (axp->lu.x->optyp == OPEMPTY) continue;
   switch ((byte) i) {
    case 1: xsp->ap[0] = (word) is_forced; break;
    case 2: xsp->ap[0] = (word) numdrvs; break;
    /* next 3 are 0, 1, and x drivers */
    case 3: xsp->ap[0] = (word) drvcnt[0]; break;
    case 4: xsp->ap[0] = (word) drvcnt[1]; break;
    case 5: xsp->ap[0] = (word) drvcnt[3]; break;
    default: __case_terr(__FILE__, __LINE__);
   }
   __exec2_proc_assign(axp->lu.x, xsp->ap, xsp->bp);
  }
just_ret:
 xsp->ap[0] = (numdrvs > 1) ? 1 : 0;
}


/*
 * execute the test plus args system function
 * argument does not include the plus
 */
static void exec_testplusargs(struct expr_t *ndp)
{
 int slen;
 register struct optlst_t *olp;
 register char *chp, *argchp;
 int rv;
 struct xstk_t *xsp;

 argchp = __get_eval_cstr(ndp->ru.x->lu.x, &slen);
 for (rv = 0, olp = __opt_hdr; olp != NULL; olp = olp->optlnxt)
  {
   /* ignore markers added for building vpi argc/argv */
   if (olp->is_bmark || olp->is_emark) continue;

   chp = olp->opt;
   if (*chp != '+') continue;
   if (strcmp(&(chp[1]), argchp) == 0) { rv = 1; break; }
  }
 push_xstk_(xsp, WBITS);
 xsp->bp[0] = 0L;
 xsp->ap[0] = (word) rv;
 __my_free(argchp, slen + 1);
}

/*
 * execute the scan plus args added system function 
 * same function as mc_scan_plusargs but assigns to 2nd parameter
 * almost same code as mc scan plus args pli system task
 * argument does not include the '+'
 */
static void exec_scanplusargs(struct expr_t *ndp)
{
 register struct optlst_t *olp;
 register char *chp;
 int arglen, rv;
 struct expr_t *fax;
 struct xstk_t *xsp;
 char *plusarg;
 
 fax = ndp->ru.x;
 /* this is the passed argment prefix */
 plusarg = __get_eval_cstr(fax->lu.x, &arglen);

 /* all options expanded and saved so this is easy */
 for (rv = 0, olp = __opt_hdr; olp != NULL; olp = olp->optlnxt)
  {
   /* ignore markers added for building vpi argc/argv */
   if (olp->is_bmark || olp->is_emark) continue;

   chp = olp->opt;
   if (*chp != '+') continue;

   /* option length if the length of the command line plus option string */
   /* option must be at least as long as passed arg or cannot match */
   if (strlen(chp) < arglen) continue; 
   /* match prefix - arg. is same or narrow that plus command line option */
   if (strncmp(&(chp[1]), plusarg, arglen) == 0)
    {
     rv = 1;
     xsp = __cstr_to_vval(&(chp[arglen + 1]));
     fax = fax->ru.x;
     if (xsp->xslen != fax->lu.x->szu.xclen)
      __sizchgxs(xsp, fax->lu.x->szu.xclen);
     __exec2_proc_assign(fax->lu.x, xsp->ap, xsp->bp);
     __pop_xstk();
     break;
    }
  }
 push_xstk_(xsp, WBITS);
 xsp->bp[0] = 0L;
 xsp->ap[0] = (word) rv;
 __my_free(plusarg, arglen + 1);
}

/*
 * execute 1 real in returns real transcendental
 * places computed real on to expr stack
 */
static void exec_1arg_transcendental(int syfnum, struct expr_t *fax)
{
 double d1, d2;
 struct xstk_t *xsp;

 /* this pushes avaluated expressions onto stack - always real and replaced */
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));

 /* DBG - LOOKATME - why here 
 if (finite(d1) == 0) __arg_terr(__FILE__, __LINE__);
 -- */
 switch (syfnum) {
  case STN_COS: d2 = cos(d1); break;
  case STN_SIN: d2 = sin(d1); break;
  case STN_TAN: d2 = tan(d1); break;
  case STN_ACOS:
   if (d1 < -1.0 || d1 > 1.0)
    {
     __sgfwarn(631,
      "$acos system function argument %s outside -1 to 1 legal range - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = acos(d1);
   break;
  case STN_ACOSH:
   if (d1 < 1.0)
    {
     __sgfwarn(631,
      "$acosh system function argument %s outside 1 to inf legal range - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = acosh(d1);
   break;
  case STN_ASIN:
   if (d1 < -1.0 || d1 > 1.0)
    {
     __sgfwarn(631,
      "$asin system function argument %s outside -1 to 1 legal range - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = asin(d1);
   break;
  case STN_ASINH: d2 = asinh(d1); break;
  case STN_ATAN: d2 = atan(d1); break;
  case STN_COSH: d2 = cosh(d1); break;
  case STN_SINH: d2 = sinh(d1); break;
  case STN_TANH: d2 = tanh(d1); break;
  case STN_ATANH:
   if (d1 < -1.0 || d1 > 1.0)
    {
     __sgfwarn(631,
      "$atanh system function argument %s outside -1 to 1 legal range - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = atanh(d1);
   break;
  case STN_LN:
   if (d1 <= 0.0)
    {
     __sgfwarn(631,
      "$ln system function argument %s illegal non positive - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = log(d1);
   break;
  case STN_LOG10:
   if (d1 <= 0.0)
    {
     __sgfwarn(631,
      "$log10 system function argument %s illegal non positive - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = log10(d1);
   break;
  case STN_ABS: d2 = fabs(d1); break;
  case STN_SQRT:
   if (d1 < 0.0)
    {
     __sgfwarn(631,
      "$sqrt system function argument %s illegal negative - returning 0.0", 
       __msgexpr_tostr(__xs, fax));
     d2 = 0.0;
    }
   else d2 = sqrt(d1);
   break;
  case STN_EXP: d2 = exp(d1); break;
  case STN_HSQRT:
   if (d1 >= 0.0) d2 = sqrt(d1); else d2 = -sqrt(-d1);
   break;
  case STN_HLOG:
   if (d1 > 0.0) d2 = log(d1);
   else if (d1 == 0.0) d2 = 0.0;
   else d2 = -log(-d1);
   break;
  case STN_HLOG10:
   if (d1 > 0.0) d2 = log10(d1);
   else if (d1 == 0.0) d2 = 0.0;
   else d2 = -log10(-d1);
   break;
  case STN_HDB: 
   if (d1 > 0.0) d2 = log(d1);
   else if (d1 == 0.0) d2 = 0.0;
   else d2 = -20.0*log(-d1);
   break;
  default: d2 = 0.0; __case_terr(__FILE__, __LINE__);
 }
 memcpy(xsp->ap, &d2, sizeof(double));
 xsp->bp = &(xsp->ap[1]);
 xsp->xslen = WBITS;
}

/*
 * execute transcendental int (convert to int) routine
 */
static void exec_transcendental_int(struct expr_t *ndp)
{
 int ival;
 double d1;
 struct expr_t *fax;
 struct xstk_t *xsp;

 /* this returns 32 bit signed reg aka integer not real */
 fax = ndp->ru.x->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real)
  {
   if (xsp->xslen != WBITS) __sizchgxs(xsp, WBITS);
   if (xsp->bp[0] != 0) { xsp->ap[0] = ALL1W; xsp->bp[0] = ALL1W; }
   else 
    {
     /* LOOKATME - does this do anything? */ 
     ival = (int) xsp->ap[0];
     xsp->ap[0] = (word) ival;
    }
  }
 else
  {
   memcpy(&d1, xsp->ap, sizeof(double));
   ival = (int) d1;
   xsp->bp[0] = 0L;
   xsp->ap[0] = (word) ival;
  }
 /* reuse xstk that know is now WBITS */ 
}

/*
 * execute transcendental sign routine
 */
static void exec_transcendental_sign(struct expr_t *ndp)
{
 int ival;
 double d1;
 struct expr_t *fax;
 struct xstk_t *xsp;

 /* this returns 32 bit signed reg aka integer not real */
 fax = ndp->ru.x->lu.x;

 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));
 if (d1 < 0) ival = -1; else if (d1 > 0) ival = 1; else ival = 0;
 /* reuse xstk that know is WBITS */ 
 xsp->bp[0] = 0L;
 xsp->ap[0] = (word) ival;
}

/*
 * exec transcendental pow - takes 2 args
 * also hspice sign with 2 args here 
 */
static void exec_transcendental_powsign(int sysfnum, struct expr_t *ndp)
{
 int ival;
 double d1, d2, d3;
 struct expr_t *fax, *fax2;
 struct xstk_t *xsp;

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));
 __pop_xstk();

 ndp = ndp->ru.x;
 fax2 = ndp->lu.x;
 xsp = __eval_xpr(fax2);
 if (!fax2->is_real) d2 = __cnvt_stk_to_real(xsp, (fax2->has_sign == 1));
 else memcpy(&d2, xsp->ap, sizeof(double));

 d3 = 0.0;
 if (sysfnum == STN_POW)
  {
   if (d1 < 0.0)
    {
     double d4;

     /* notice this uses hspice not Verilog conversion to int - matters not */
     ival = (int) d2;
     d4 = ival;
     if (d4 != d2)
      {
       __sgfwarn(631,
        "$pow system function argument first argument %s negative and second argument %s non integral - returning 0.0", 
       __msgexpr_tostr(__xs, fax), __msgexpr_tostr(__xs2, fax2));
       d2 = 0.0;
      }
     else d3 = pow(d1, d2);
    }
   else d3 = pow(d1, d2);
  }
 else if (sysfnum == STN_HPOW)
  {
   /* LOOKATME - notice this uses Hspice not Verilog conversion to int */
   ival = (int) d2;
   d2 = ival;
   d3 = pow(d1, d2);
  }
 else if (sysfnum == STN_HPWR)
  {
   if (d1 > 0.0) d3 = pow(d1, d2);
   else if (d1 == 0.0) d3 = 0.0;
   else d3 = -pow(-d1, d2);
  }
 else if (sysfnum == STN_HSIGN)
  {
   /* notice $hsign returns double but $sign returns int */
   if (d2 > 0.0) d3 = fabs(d1);
   else if (d2 == 0.0) d3 = 0.0;
   else d3 = -fabs(d1);
  }
 else { d3 = 0.0; __case_terr(__FILE__, __LINE__); }

 memcpy(xsp->ap, &d3, sizeof(double));
 xsp->bp = &(xsp->ap[1]);
 xsp->xslen = WBITS;
}

/*
 * exec transcendental min/max - takes 2 args
 * LOOKATME - since can do with arg macro maybe unneeded
 */
static void exec_transcendental_minmax(int syfnum, struct expr_t *ndp)
{
 double d1, d2, d3;
 struct expr_t *fax;
 struct xstk_t *xsp;

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));
 __pop_xstk();

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d2 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d2, xsp->ap, sizeof(double));

 if (syfnum == STN_MIN) d3 = (d1 < d2) ? d1 : d2;
 else d3 = (d1 > d2) ? d1 : d2;

 memcpy(xsp->ap, &d3, sizeof(double));

 xsp->bp = &(xsp->ap[1]);
 xsp->xslen = WBITS;
}

/*
 * exec transcendental atan2 - takes 2 args
 */
static void exec_transcendental_atan2(struct expr_t *ndp)
{
 double d1, d2, d3;
 struct expr_t *fax;
 struct xstk_t *xsp;

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));
 __pop_xstk();

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d2 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d2, xsp->ap, sizeof(double));

 d3 = atan2(d1, d2);
 memcpy(xsp->ap, &d3, sizeof(double));

 xsp->bp = &(xsp->ap[1]);
 xsp->xslen = WBITS;
}

/*
 * exec transcendental hypot (dist func) - takes 2 args
 */
static void exec_transcendental_hypot(struct expr_t *ndp)
{
 double d1, d2, d3;
 struct expr_t *fax;
 struct xstk_t *xsp;

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d1 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d1, xsp->ap, sizeof(double));
 __pop_xstk();

 ndp = ndp->ru.x;
 fax = ndp->lu.x;
 xsp = __eval_xpr(fax);
 if (!fax->is_real) d2 = __cnvt_stk_to_real(xsp, (fax->has_sign == 1));
 else memcpy(&d2, xsp->ap, sizeof(double));

 d3 = hypot(d1, d2);
 memcpy(xsp->ap, &d3, sizeof(double));

 xsp->bp = &(xsp->ap[1]);
 xsp->xslen = WBITS;
}


/*
 * ROUTINES TO EXEC CAUSE
 */

/*
 * execute a cause statement
 * this is simply an assign to the event variable - gets added to net chg
 * as normal var change and during prep has normal dce npp's built
 */
static void exec_cause(struct st_t *stp)
{
 int nd_itpop;
 struct expr_t *xp;
 struct net_t *np;
 struct gref_t *grp;

 if (__st_tracing)
  __tr_msg("trace: %-7d -> %s\n", __slin_cnt,
   __to_idnam(stp->st.scausx));
 xp = stp->st.scausx;
 nd_itpop = FALSE;
 if (xp->optyp == GLBREF)
  {
   /* idea for xmr cause is to cause an event in some other part of the */
   /* itree - by changine current itree place will match waits only in */
   /* target of cause instance */ 
   grp = xp->ru.grp;    
   __xmrpush_refgrp_to_targ(grp);
   nd_itpop = TRUE;
  }
 else if (xp->optyp != ID) __case_terr(__FILE__, __LINE__);

 /* notice even if global ref. can still get net from symbol */ 
 np = xp->lu.sy->el.enp;
 /* notice cause does nothing - just schedules trigger for each waiting */
 /* armed ectrl - so any waiting event control blocks will be activated */

 /* record cause event var change as usual, if no pending do not record */
 /* must not record or will be worse event ordering dependency */
 /* know change see if need to record - also maybe dmpvars */
 record_nchg_(np);

 if (nd_itpop) __pop_itstk();
}

/*
 * DISABLE ROUTINES
 */

/*
 * ROUTINES TO IMPLEMENT TASK AND THREAD DISABLING
 */

/*
 * execute a disable statement - disable tskp
 * return T to cause disable of current thread (above on chain)
 * and F if this thread continues as usual
 * 
 * this code is not for functions there all disables converted to added gotos
 * disable argument is task name not ?? 
 */
extern int __exec_disable(struct expr_t *dsxndp)
{
 register struct tskthrd_t *ttp, *ttp_real_r;
 int thread_finished, nd_itpop; 
 struct sy_t *syp;
 struct task_t *tskp; 
 struct thread_t *dsathp, *thp, *thd_1up, *sav_thd;
 struct gref_t *grp;
 struct st_t *stp;

 /* this pointer to target symbol in some other module */
 nd_itpop = FALSE;
 syp = dsxndp->lu.sy;  
 if (dsxndp->optyp == GLBREF)
  { grp = dsxndp->ru.grp; __xmrpush_refgrp_to_targ(grp); nd_itpop = TRUE; }

 /* disabling every thread associated with task of given instance required */
 /* first disable and free all underneath */ 
 tskp = syp->el.etskp;
 /* assume current thread not disabled */
 thread_finished = FALSE;
 /* task can be enabled from >1 place in inst. but share vars */
 ttp = tskp->tthrds[__inum];

 if (ttp == NULL)
  { 
   __sgfinform(469, "disable of %s %s no effect - not active",
    __to_tsktyp(__xs2, tskp->tsktyp), __msg_blditree(__xs, __inst_ptr, tskp));
   goto done;
  }
 for (; ttp != NULL;)
  {
   /* the task thread list that ttp points to will probably be freed */
   /* changing the right ptr so must get the next before freeing */
   ttp_real_r = ttp->tthd_r;
   dsathp = ttp->tthrd;

   /* better point back to self */
   /* -- DBG remove */
   if (dsathp->assoc_tsk != NULL && dsathp->assoc_tsk != tskp)
    __misc_sgfterr(__FILE__, __LINE__);
   /* --- */

   /* first if recurive task enables, disable top most */
   if ((thp = find_hgh_sametskthrd(dsathp)) != NULL) dsathp = thp;

   /* case 1: disabling current thread */ 
   if (dsathp == __cur_thd)
    {
     /* cannot unlink here since will be unlinked because thread done */
     __cur_thd->th_dsable = TRUE;
     thread_finished = TRUE;
    }
   /* case 2: disabling highest thread above currently active */
   /* this is case where above spawns subthread for task/blk/fj */
   /* case 1: and 2: mutually exclusive */
   else if (thread_above_cur(dsathp))
    {
     /* free under threads including current */
     free_thd_subtree(dsathp);
     /* ok to just set current thread since finished and all under gone */
     __cur_thd = dsathp;
     __cur_thd->th_dsable = TRUE;
     thread_finished = TRUE;
    }
   /* disabling thread with scheduled event elsewhere in thread tree */
   else
    {
     thd_1up = dsathp->thpar;
     /* anything (task-named block than can be disabled here parent */
     /* DBG remove --- */
     if (thd_1up == NULL) __misc_terr(__FILE__, __LINE__);
     if (thd_1up->thofscnt > 1 && !dsathp->th_fj)
      __misc_terr(__FILE__, __LINE__);  
     /* --- */
     /* disable a thread means remove and unlink from parent */
     thd_1up->thofscnt -= 1;
     if (dsathp->thleft != NULL) dsathp->thleft->thright = dsathp->thright;
     /* if first thread finished, make up thofs list point to its right */
     else thd_1up->thofs = dsathp->thright;
     if (dsathp->thright != NULL) dsathp->thright->thleft = dsathp->thleft;

     /* free all under and thread itself - mark schd. events canceled */ 
     free_thd_subtree(dsathp);
     __free_1thd(dsathp);
     dsathp = NULL;
     /* if more subthreads, nothing to do since will eval thread events */
     /* for other fj subthreads */
     if (thd_1up->thofscnt == 0)
      {
       /* know have parent or cannot get here */
       /* previously disable thread scheduled, after remove schedule 1 up */
       /* eliminating any waiting delay/event controls */
       sav_thd = __cur_thd;
       __cur_thd = thd_1up;
       /* if this was task with output parameters - do not store and */
       /* make sure up thread not in task returning state */
       if (thd_1up->th_postamble)
        { stp = thd_1up->thnxtstp->stnxt; thd_1up->th_postamble = FALSE; }
       else stp = thd_1up->thnxtstp;
       /* this will incorrectly turn on stmt suspend */ 
       /* DBG remove --- */
       if (__stmt_suspend) __misc_terr(__FILE__, __LINE__);
       /* --- */ 
       suspend_curthd(stp);
       __stmt_suspend = FALSE;
       __cur_thd = sav_thd;
      }
    }
   /* move to next - saved since previous probably freed */
   ttp = ttp_real_r;
  }

done:
 /* DBG remove ---
 __dmp_tskthd(tskp, __inst_mod);
 --- */
 if (nd_itpop) __pop_itstk();
 return(thread_finished);
}

/*
 * find highest thread associated with same task above
 * return NULL if thread to disable not above this one 
 */
static struct thread_t *find_hgh_sametskthrd(struct thread_t *dsthp)
{
 register struct thread_t *thp;
 struct thread_t *highthd;

 for (highthd = NULL, thp = __cur_thd; thp != NULL; thp = thp->thpar)
  {
   if (thp->assoc_tsk != NULL && thp->assoc_tsk == dsthp->assoc_tsk)
    highthd = thp;
  }
 return(highthd);
}

/*
 * return T if thread is above current thread
 */
static int thread_above_cur(struct thread_t *dsthp)
{
 register struct thread_t *thp;

 for (thp = __cur_thd; thp != NULL; thp = thp->thpar)
  {
   if (thp == dsthp) return(TRUE);
  }
 return(FALSE);
}

/*
 * free a thread subtree below thp (but not including thp)
 */
static void free_thd_subtree(struct thread_t *thp)
{
 if (thp->thofs != NULL) __free_thd_list(thp->thofs);
 thp->thofscnt = 0;
 thp->thofs = NULL;
 thp->th_fj = FALSE;
}

/*
 * free a thread list (passed head that is probably thofs)
 */
extern void __free_thd_list(struct thread_t *thp)
{
 register struct thread_t *thp2, *thp3;

 for (thp2 = thp; thp2 != NULL;)
  {
   thp3 = thp2->thright;
   if (thp2->thofs != NULL) __free_thd_list(thp2->thofs);
   __free_1thd(thp2);
   thp2 = thp3;
  }
}

/*
 * free one thread
 * called after any subthreads freed
 */
extern void __free_1thd(struct thread_t *thp)
{
 /* every thread with an associated task - unlabeled fork-join will not */
 /* DBG remove --- */
 if (thp->th_itp == NULL || thp->th_ialw)
   __misc_terr(__FILE__, __LINE__);
 /* --- */
 __push_itstk(thp->th_itp);
 free_thd_stuff(thp);
 __pop_itstk();
 __my_free((char *) thp, sizeof(struct thread_t)); 
}

/*
 * routine used by disable to force finish (free all but thread itself)
 */
static void free_thd_stuff(struct thread_t *thp)
{
 /* every thread with an associated task - unlabeled fork-join will not */
 if (thp->tthlst != NULL) unlink_tskthd(thp);
 if (thp->thdtevi != -1) __tevtab[thp->thdtevi].te_cancel = TRUE;
 /* notice if free statements (from iact) this will be set to nil */
 /* since when freeing dctrl and wait freeing the dctp */
 /* events freed later */
 if (thp->th_dctp != NULL)
  {
   if (thp->th_dctp->dceschd_tevs != NULL)
    thp->th_dctp->dceschd_tevs[thp->th_itp->itinum] = -1;
   thp->th_dctp = NULL;
  }
 if (thp->th_rhsform) 
  {
   __my_free((char *) thp->th_rhswp, 2*WRDBYTES*thp->th_rhswlen);
   thp->th_rhswp = NULL;
  }  
}

/*
 * unlink and free one task thead - remove from tasks list
 *
 * every thread that has assoc task has pointer to 1 tskthrd_t element
 * that is its entry on task's thread list for given instance 
 */
static void unlink_tskthd(struct thread_t *thp)
{
 struct tskthrd_t *ttp;
 struct task_t *tskp;

 ttp = thp->tthlst;
 /* DBG remove --- */
 if (__debug_flg && __st_tracing)
  {
   /* unlink of disabled thread to with no assoc. task */
   /* should emit the itree loc. here */
   if (thp->assoc_tsk == NULL) __misc_sgfterr(__FILE__, __LINE__);
   __tr_msg("++ unlink task %s for instance %s number %d\n",
    thp->assoc_tsk->tsksyp->synam, __inst_ptr->itip->isym->synam, __inum);
  }
 /* --- */

 /* lifo recursive enable freeing case */
 if (ttp->tthd_l == NULL)
  {
   tskp = thp->assoc_tsk;
   tskp->tthrds[__inum] = ttp->tthd_r;
   if (ttp->tthd_r != NULL) ttp->tthd_r->tthd_l = NULL;
  }
 else
  {
   /* any other order */
   ttp->tthd_l->tthd_r = ttp->tthd_r;
   if (ttp->tthd_r != NULL) ttp->tthd_r->tthd_l = ttp->tthd_l;
  }
 /* ttp has already been linked out */
 __my_free((char *) ttp, sizeof(struct tskthrd_t));
 thp->tthlst = NULL;   
 /* DBG remove ---
 __dmp_tskthd(tskp, __inst_ptr->itip->imsym->el.emdp);
 --- */
}

/*
 * SYSTEM TASK/FUNCTION EXECUTION ROUTINES
 */

/* 
 * execute the system tasks
 *
 * for monitor and strobe effect is to set up later action
 */
extern struct st_t *__exec_stsk(struct st_t *stp, struct sy_t *tsyp,
 struct tskcall_t *tkcp)
{
 int base, stav, oslen, slen;
 word wval;
 struct systsk_t *stbp;
 struct strblst_t *strblp;
 struct expr_t *argvx;
 char *chp;

 stbp = tsyp->el.esytbp;
 switch (stbp->stsknum) {
  /* file manipulation - most functions */
  case STN_FCLOSE:
   mcd_do_fclose(tkcp->targs);
   break;
  /* display write to terminal */
  case STN_DISPLAY: base = BDEC; goto nonf_disp;
  case STN_DISPLAYB: base = BBIN; goto nonf_disp;
  case STN_DISPLAYH: base = BHEX; goto nonf_disp;
  case STN_DISPLAYO:
   base = BOCT;
nonf_disp:
   __do_disp(tkcp->targs, base);
   __cvsim_msg("\n");
   break;

  /* write to terminal with no ending nl */
  case STN_WRITE: base = BDEC; goto nonf_write;
  case STN_WRITEH: base = BHEX; goto nonf_write;
  case STN_WRITEB: base = BBIN; goto nonf_write;
  case STN_WRITEO: base = BOCT;
nonf_write:
   __do_disp(tkcp->targs, base);
   /* if tracing to stdout need the new line just to stdout */
   /* FIXME - should change to separate verilog.trace file ?? */
   /* NOTICE - this is not __tr_msg */
   if (__st_tracing && __tr_s == stdout) __cv_msg("\n");
   break;

  /* multi-channel descriptor display to file */
  case STN_FDISPLAY: base = BDEC; goto f_disp;
  case STN_FDISPLAYB: base = BBIN; goto f_disp;
  case STN_FDISPLAYH: base = BHEX; goto f_disp;
  case STN_FDISPLAYO:
   base = BOCT;
f_disp:
   __fio_do_disp(tkcp->targs, base, TRUE, tsyp->synam);
   break;

  /* multi-channel descriptor write to file */
  case STN_FWRITE: base = BDEC; goto f_write;
  case STN_FWRITEH: base = BHEX; goto f_write;
  case STN_FWRITEB: base = BBIN; goto f_write;
  case STN_FWRITEO: base = BOCT;
f_write:
   /* if tracing need the new line */
   __fio_do_disp(tkcp->targs, base, __st_tracing, tsyp->synam);
   break;
  /* like display except write at end of current time */
  case STN_FSTROBE: case STN_FSTROBEH: case STN_FSTROBEB: case STN_FSTROBEO:
  case STN_STROBE: case STN_STROBEH: case STN_STROBEB: case STN_STROBEO:
   /* if same strobe statement repeated in one time slot - warn/inform */
   if (stp->strb_seen_now)
    { 
     /* if dup. of same inst. and statement, do not re-add */
     if (!chk_strobe_infloop(stp, tsyp)) break;
    }
   if (__strb_freelst != NULL) 
    {
     strblp = __strb_freelst;
     __strb_freelst = __strb_freelst->strbnxt;
    }
   else strblp = (struct strblst_t *) __my_malloc(sizeof(struct strblst_t));
   strblp->strbstp = stp;
   stp->strb_seen_now = TRUE;
   strblp->strb_itp = __inst_ptr;
   strblp->strbnxt = NULL;
   __iact_can_free = FALSE;
   if (__strobe_hdr == NULL)
    {
     __strobe_hdr = __strobe_end = strblp; 
     __slotend_action |= SE_STROBE;
    }
   else { __strobe_end->strbnxt = strblp; __strobe_end = strblp; }
   break;
  /* monitor control sys tasks */ 
  case STN_MONITOROFF: __monit_active = FALSE; break;
  case STN_MONITORON:
   __monit_active = TRUE;
   __iact_can_free = FALSE;
   /* when monitor turned on (even if on), trigger even if no changes */
   /* and update save dce values to current */
   __slotend_action |= SE_MONIT_CHG;
   break;
  case STN_FMONITOR: case STN_FMONITORH: case STN_FMONITORB:
  case STN_FMONITORO:
   __start_fmonitor(stp); 
   __iact_can_free = FALSE;
   /* DBG remove ---
   if (__debug_flg)
    __dmpmod_nplst(__inst_mod, TRUE); 
   -- */
   break;
  /* change monitor write to terminal system tasks */
  case STN_MONITOR: case STN_MONITORH: case STN_MONITORB: case STN_MONITORO:
   __start_monitor(stp);
   __iact_can_free = FALSE;
   break;
  /* time releated system tasks */
  case STN_PRINTTIMESCALE:
   exec_prttimscale(tkcp->targs);
   break;
  case STN_TIMEFORMAT:
   /* if no `timescale in design, $timeformat is a noop */
   if (__des_has_timescales) exec_timefmt(tkcp->targs);
   break;
  case STN_READMEMB:
   __exec_readmem(tkcp->targs, BBIN);
   break;
  case STN_READMEMH:
   __exec_readmem(tkcp->targs, BHEX);
   break;
  case STN_SREADMEMB:
   __exec_sreadmem(tkcp->targs, BBIN);
   break;
  case STN_SREADMEMH:
   __exec_sreadmem(tkcp->targs, BHEX);
   break;

  /* dump variables tasks */
  case STN_DUMPVARS:
   __exec_dumpvars(tkcp->targs);
   break;
  case STN_DUMPALL:
   if (__dv_state == DVST_NOTSETUP)
    {
     __sgferr(703, "$dumpall ignored because: $dumpvars not set up");
     break;
    }
   /* dumpall is independent of other dumpvars except past file size limit */
   /* but still must dump at end of time slot */
   if ((__slotend_action & SE_DUMPALL) != 0)
    __sgfinform(445,
     "$dumpall ignored beause: $dumpall already executed at this time");
   /* turn on need dumpall plus need some dump vars action */
   __slotend_action |= (SE_DUMPALL | SE_DUMPVARS);
   break;
  case STN_DUMPFILE: 
   /* SJM 10/26/00 - fix to match XL */
   /* can set dumpvars file name before time of dumpvars setup */
   /* but not after dumpvars started */
   if (__dv_seen)
    {
     __sgferr(1066,
      "$dumpfile name set at time after $dmmpvars started - name not changed");
     break;
    }
   /* if no argument just leaves default - cannot be called more than once */
   if (tkcp->targs == NULL) break;

   /* set the file to dump too */
   argvx = tkcp->targs->lu.x;
   chp = __get_eval_cstr(argvx, &slen);
   /* cannot open yet but must save */
   oslen = strlen(__dv_fnam);
   if (__dv_fnam != NULL) __my_free((char *) __dv_fnam, oslen + 1);
   __dv_fnam = chp;
   break;
  case STN_DUMPFLUSH:
   /* flush the file now since need to flush before adding this times */
   /* dumpvars */
   if (__dv_state == DVST_NOTSETUP)
    {
     __sgferr(703,
      "$dumpflush ignored because: dumping of variables not begun");
     break;
    }
   if (__dv_fd == -1) __arg_terr(__FILE__, __LINE__);
   /* flush when called - i.e. flush is before this time's dumping */
   /* this call OS functions that may set errno */
   __my_dv_flush();
   break;
  case STN_DUMPLIMIT:
   /* notice can call even if not set up and can change if not over limit */
   argvx = tkcp->targs->lu.x;
   if (!__get_eval_word(argvx, &wval) || ((wval & (1 << (WBITS - 1))) != 0))
    {
     __sgferr(1036, "$dumplimit value %s illegal positive integer - not set",
      __msgexpr_tostr(__xs, tkcp->targs));
     break;
    }
    
   /* if already over limit cannot change */
   if (__dv_state == DVST_OVERLIMIT)
    {
     __sgferr(1069,
      "$dumplimit not set to %d - dump file already over previous limit %d",
      (int) wval, __dv_dumplimit_size);
     break;
    }
   /* else inform if already set */
   if (__dv_dumplimit_size != 0)
    {
     __sgfinform(449, "$dumplimit changed from %d to %d",
      __dv_dumplimit_size, (int) wval);
    }
   __dv_dumplimit_size = (int) wval; 
   break;
  case STN_DUMPON: 
   switch ((byte) __dv_state) {
    case DVST_NOTSETUP:
     __sgferr(703, "$dumpon ignored because: $dumpvars not set up");
     break;
    /* if over limit silently ignore */
    case DVST_OVERLIMIT: break;
    case DVST_DUMPING:
     if ((__slotend_action & SE_DUMPOFF) != 0)
      {
       __sgfinform(453, "$dumpon overrides $dumpoff executed at this time");
       __slotend_action &= ~SE_DUMPOFF;
      }
     __sgfinform(446, "$dumpon ignored because: dumping already on");
     break;
    case DVST_NOTDUMPING:
     if ((__slotend_action & SE_DUMPON) != 0)
      __sgfinform(445,
       "$dumpon ignored because: $dumpon already executed at this time");
     /* also indicate need some dumpvars action */
     __slotend_action |= (SE_DUMPON | SE_DUMPVARS);
     break;
    default: __case_terr(__FILE__, __LINE__);
   }
   break;
  case STN_DUMPOFF: 
   switch ((byte) __dv_state) {
    case DVST_NOTSETUP:
     __sgferr(703, "$dumpoff ignored because: $dumpvars not set up");
     break;
    case DVST_OVERLIMIT: break;
    case DVST_NOTDUMPING:
     if ((__slotend_action & SE_DUMPON) != 0)
      {
       __sgfinform(453, "$dumpoff overrides $dumpon executed at this time");
       __slotend_action &= ~SE_DUMPON;
      }
     __sgfinform(446, "$dumpoff ignored because: dumping already off");
     break;
    case DVST_DUMPING:
     if ((__slotend_action & SE_DUMPON) != 0)
      __sgfinform(445,
       "$dumpoff ignored because: $dumpoff already executed at this time");
     /* also indicate need some dumpvars action even if no changes */
     __slotend_action |= (SE_DUMPOFF | SE_DUMPVARS);
     break;
    default: __case_terr(__FILE__, __LINE__); 
   }
   break;
  case STN_INPUT:
   __exec_input_fnamchg(tkcp->targs);
   break;
  case STN_HISTORY:
   /* LRM strictly requires all element here */
   __exec_history_list(__hist_cur_listnum);
   break;
  case STN_NOKEY:
no_key:
   __sgfwarn(560, "%s no effect - obsolete key file not supported",
    tsyp->synam);
   /* DBGER ---
   __save_key_s = __key_s;
   __nokey_seen = TRUE;
   __key_s = NULL;
   break;
   -- */
  case STN_KEY:
   goto no_key;
  case STN_KEEPCMDS: 
   if (__history_on)
    __sgfinform(458, "%s ignored - command history saving already on", 
     tsyp->synam);
   __history_on = TRUE; 
   break;
  case STN_NOKEEPCMDS: 
   if (!__history_on)
    __sgfinform(458, "%s ignored - command history saving already off", 
     tsyp->synam);
   __history_on = FALSE; 
   break;
  case STN_NOLOG:
   if (__log_s != NULL) fflush(__log_s);
   __save_log_s = __log_s;   
   
   /* notice cannot close log file */
   /* SJM 03/26/00 - 2 in Verilog 2000 not used for log file lumped with */
   /*  stdout (bit 0 value 1) */
   /* ---
   if (__mulchan_tab[2].mc_s != NULL) 
    {
     __mulchan_tab[2].mc_s = NULL;
     chp = __mulchan_tab[2].mc_fnam;
     __my_free(chp, strlen(chp) + 1);
     __mulchan_tab[2].mc_fnam = NULL;
    }
   --- */
   __log_s = NULL;
   break;
  case STN_LOG:
   exec_log_fnamchg(tkcp->targs); 
   break;
  case STN_TRACEFILE:
   argvx = tkcp->targs->lu.x;
   exec_trace_fnamchg(argvx); 
   break;
  case STN_SCOPE:
   exec_expr_schg(tkcp->targs->lu.x);
   break;
  /* listing off because no interactive environment */
  case STN_LIST:
   /* list current scope - */
   if (tkcp->targs != NULL)
    {
     struct itree_t *sav_scope_ptr = __scope_ptr;
     struct task_t *sav_tskp = __scope_tskp;

     exec_expr_schg(tkcp->targs->lu.x);
     __do_scope_list();
     __scope_ptr = sav_scope_ptr; 
     __scope_tskp = sav_tskp;
    }
   else __do_scope_list();
   break;

  /* unimplemented - need complicated state write file mechanism */
  case STN_SAVE: case STN_INCSAVE: case STN_RESTART:
   goto un_impl;

  case STN_FINISH:
   if (__tfrec_hdr != NULL) __call_misctfs_finish();
   if (__have_vpi_actions) __vpi_endsim_trycall();
   stav = get_opt_starg(tkcp->targs, 1); 
   if (stav >= 1 || __verbose)
    {
     /* FIXME - why am i needed - if (!__quiet_msgs) __cv msg("\n"); */
     __cv_msg("Halted at location %s time %s from call to $finish.\n",   
      __bld_lineloc(__xs2, (unsigned) __sfnam_ind, __slin_cnt),
      __to_timstr(__xs, &__simtime));
    }
   if (stav >= 2 || __verbose) __emit_stsk_endmsg();
   /* notice must always print error counts if any */
   if (__pv_err_cnt != 0 || __pv_warn_cnt != 0 || __inform_cnt != 0)
    __cv_msg("  There were %d error(s), %d warning(s), and %d inform(s).\n",
     __pv_err_cnt, __pv_warn_cnt, __inform_cnt);
   __my_exit(0, TRUE);
  case STN_STOP:
   if (__no_iact)
    {
     __sgfwarn(560, "%s no effect - interactive environment disabled",
      stbp->stsknam);
     break;
    }
   if (__iact_state)
    {
     __sgfwarn(587, "%s no effect - enabled from interactive debugger",
      stbp->stsknam);
     break;
    }
   stav = get_opt_starg(tkcp->targs, 1); 
   if (stav >= 1)
    {
     if (!__quiet_msgs) __cv_msg("\n");
     __cv_msg(
      "$stop executed at time %s from source location %s.\n",
      __to_timstr(__xs, &__simtime), __bld_lineloc(__xs2,
      (unsigned) __sfnam_ind, __slin_cnt));
    }
   if (stav >= 2) __emit_stsk_endmsg();
   __pending_enter_iact = TRUE;
   __iact_reason = IAER_STOP;
   signal(SIGINT, SIG_IGN);
   __stmt_suspend = TRUE;
   suspend_curthd(stp->stnxt);
   return(NULL);
  case STN_SETTRACE:
   /* statement tracing change requires suspend of thread */
   __st_tracing = TRUE;
   /* if enabled from interactive state, suspend will cause core dump */
   if (!__iact_state) suspend_curthd(stp->stnxt);
   /* if trace file name set from command option open now if not open */  
   __maybe_open_trfile();
   return(NULL);
  case STN_CLEARTRACE: 
   __st_tracing = FALSE;
   break;
  case STN_SETEVTRACE:
   __ev_tracing = TRUE;
   /* if trace file name set from command option open now if not open */  
   __maybe_open_trfile();
   break;
  case STN_CLEAREVTRACE:
   /* notice leave file open */
   __ev_tracing = FALSE;
   break;
  case STN_SETDEBUG:
   __debug_flg = TRUE;
   break;
  case STN_CLEARDEBUG:
   __debug_flg = FALSE;
   break;
  case STN_SHOWVARS:
   do_showvars_stask(tkcp->targs);
   break;
  case STN_SHOWVARIABLES:
   /* for now same as showvars - i.e. skip 1st control arg. */
   do_showvars_stask(tkcp->targs->ru.x);
   break;
  case STN_SYSTEM:
   /* $system with no args, means run interactive shell */ 
   if (tkcp->targs == NULL) chp = __pv_stralloc(" ");
   else
    {
     argvx = tkcp->targs->lu.x;
     if (argvx->optyp == OPEMPTY) { chp = __pv_stralloc(" "); slen = 1; }
     else chp = __get_eval_cstr(argvx, &slen);
    }
   __escape_to_shell(chp);
   slen = strlen(chp);
   __my_free(chp, slen + 1);
   break;
  case STN_SUPWARNS:
   do_warn_supp_chg(stbp->stsknam, tkcp->targs, TRUE);
   break;   
  case STN_ALLOWWARNS:
   do_warn_supp_chg(stbp->stsknam, tkcp->targs, FALSE);
   break;
  case STN_MEMUSE:
   /* this will force output */
   __cvsim_msg("Approximately %ld bytes allocated dynamic storage.\n",
     __mem_use);
   __cvsim_msg("Verilog arrays (memories) require %ld bytes.\n", __arrvmem_use);
   break;
  case STN_FLUSHLOG:
   /* LOOKATME - maybe should check for rare but possible error */
   if (__log_s != NULL) fflush(__log_s);
   if (__tr_s != NULL) fflush(__tr_s);
   break;
  case STN_RESET:
   /* do reset processing - final step is long jmp to top level */
   do_reset(tkcp->targs);
   break;
  case STN_SNAPSHOT:
   stav = get_opt_starg(tkcp->targs, DFLT_SNAP_EVS); 
   __write_snapshot(stav);
   break;
  case STN_SHOWALLINSTANCES:
   __prt2_mod_typetab(FALSE);
   break;
  case STN_SHOWSCOPES:
   do_showscopes(tkcp->targs);
   break;
  /* graphical output tasks - ignore with warn */
  case STN_GRREMOTE:
  case STN_PSWAVES:
  case STN_GRSYNCHON:
  case STN_GRREGS:
  case STN_GRWAVES:
  case STN_FREEZEWAVES:
  case STN_DEFINEGROUPWAVES:
   /* earlier warning - just ignore */
   break;
   
  /* internal simulation state printng tasks */
  case STN_SHOWEXPANDEDNETS: goto un_impl;

  /* q manipulation tasks - also q_full function */
  case STN_Q_INITIALIZE:
   do_q_init(tkcp->targs);
   break;
  case STN_Q_ADD:
   do_q_add(tkcp->targs);
   break;
  case STN_Q_REMOVE:
   do_q_remove(tkcp->targs);
   break;
  case STN_Q_EXAM:
   do_q_examine(tkcp->targs);
   break;
  case STN_SDF_ANNOTATE:
   __exec_sdf_annotate_systsk(tkcp->targs);
   break;
  default:
   /* DBG remove --- */
   if (stbp->stsknum < BASE_VERIUSERTFS || (int) stbp->stsknum > __last_systf)
     __case_terr(__FILE__, __LINE__);
   /* --- */
   /* exec (call) pli user tf system function here */ 
   if (stbp->stsknum <= __last_veriusertf) __pli_task_calltf(stp);
   else __vpi_syst_calltf(stp);
 }
 return(stp->stnxt);

un_impl:
 __sgfwarn(550, "system task %s not implemented - ignored", stbp->stsknam);
 return(stp->stnxt);
}

/*
 * MISCELLANEOUS SYSTEM TASK EXEC ROUTINES
 */

/*
 * check strobe statement repeated in same strobe system task call
 *
 * notice this only called if know strobe task enable statement repeated
 * checking for strobe repeated but in different instance 
 */
static int chk_strobe_infloop(struct st_t *stp, struct sy_t *tsksyp)
{
 register struct strblst_t *strbp;
 int match;
 
 match = FALSE;
 for (strbp = __strobe_hdr; strbp != NULL; strbp = strbp->strbnxt)
  {
   if (strbp->strbstp == stp)
    {
     match = TRUE; 
     if (strbp->strb_itp == __inst_ptr)
      {
       __sgfwarn(527, "%s enable for instance %s repeated at this time",
        tsksyp->synam, __msg2_blditree(__xs, __inst_ptr));
       return(FALSE);
      }
     /* maybe should only inform one inform per call ? */
     __sgfinform(434, "%s enable in instance %s repeated in %s at this time", 
      tsksyp->synam, __msg2_blditree(__xs, __inst_ptr),
      __msg2_blditree(__xs2, strbp->strb_itp));
    }
  }
 if (!match) __case_terr(__FILE__, __LINE__);
 return(TRUE);
}

/*
 * setup event from suspend (^c or $stop or stmt. break) of cur. thread
 * stp is place to begin after wake up
 * links new event on front of current event list
 */
static void suspend_curthd(struct st_t *stp)
{
 i_tev_ndx tevpi;

 alloc_tev_(tevpi, TE_THRD, __inst_ptr, __simtime);
 __cur_thd->thdtevi = tevpi;
 __tevtab[tevpi].tu.tethrd = __cur_thd;
 __cur_thd->thnxtstp = stp;
 __stmt_suspend = TRUE;
 __suspended_thd = __cur_thd;
 __suspended_itp = __inst_ptr;
 /* must save suspended, but popping done in event processing loop */
 __cur_thd = NULL;

 /* if hit break or step in func. save event, must unde (extr. and cancel) */
 /* to continue in function without going through event processing loop */
 if (__fcspi >= 0) __fsusp_tevpi = tevpi; else __fsusp_tevpi = -1;

 /* since just suspending and want to continue from here, put on front */
 __add_ev_to_front(tevpi);
 /* maybe remove this since - will print interactive msg anyway ? */
 if (__debug_flg && __st_tracing)
  {
   if (stp != NULL)
    sprintf(__xs2,  "at %s", __bld_lineloc(__xs, stp->stfnam_ind,
     stp->stlin_cnt));
   else strcpy(__xs2, "**past end");
   __tr_msg("-- suspend of current thread, was enabled at %s, continue %s\n",
    __bld_lineloc(__xs, __suspended_thd->thenbl_sfnam_ind,
      __suspended_thd->thenbl_slin_cnt), __xs2);
  }
}

/*
 * routine to open trace output file when needed
 */
extern void __maybe_open_trfile(void)
{
 if (strcmp(__tr_fnam, "stdout") == 0 || __tr_s != NULL) return;
 if ((__tr_s = __tilde_fopen(__tr_fnam, "w")) == NULL)
  {
   __sgferr(1247, "cannot open trace output file %s - stdout used",
    __tr_fnam);
   __tr_s = stdout;
   __my_free(__tr_fnam, strlen(__tr_fnam) + 1);
   __tr_fnam = __my_malloc(7);
   strcpy(__tr_fnam, "stdout");
  }
}

/*
 * execute the multi-channel descriptor file close 
 * this is a system task not function  
 *
 * SJM 08/09/03 - FIXME ??? - need to disable pending f monit or strobe 
 */
static void mcd_do_fclose(struct expr_t *axp)
{
 word mcd;
 struct xstk_t *xsp;

 xsp = __eval_xpr(axp->lu.x);
 if (xsp->bp[0] != 0L) 
  {
   __sgfwarn(611, 
    "$fclose multi-channel descriptor %s contains x or z bits - no action",
    __regab_tostr(__xs, xsp->ap, xsp->bp, xsp->xslen, BHEX, FALSE));
   __pop_xstk();
   return;
  }
 /* system task does not return anything but vpi_ call does */
 mcd = xsp->ap[0]; 
 __pop_xstk();
 __close_mcd((unsigned) mcd, FALSE);
}

/*
 * close mcd
 */
extern unsigned __close_mcd(unsigned mcd, int from_vpi)
{
 register int i;
 int err;

 err = FALSE;
 if (mcd == 0L)
  {
   if (!from_vpi)
    {
     __sgfinform(431,
      "$fclose passed empty (no bits on) multi-channel descriptor");
    }
   return(bld_open_mcd());
  }
 if ((mcd & 1L) != 0)  
  {
   if (!from_vpi)
    {
     __sgfinform(432,
      "$fclose bit 1 (stdout) multi-channel descriptor cannot be closed");
    }
   err = TRUE;
  }
 if ((mcd & 2L) != 0)  
  {
   if (!from_vpi)
    {
     __sgfinform(432,
      "$fclose bit 2 (stderr) multi-channel descriptor cannot be closed");
    }
   err = TRUE;
  }
 for (i = 2; i < 31; i++)
  {
   if (((mcd >> i) & 1L) == 0L) continue;

   if (__mulchan_tab[i].mc_s == NULL)
    {
     if (!from_vpi)
      {
       __sgfwarn(611,
        "$fclose multi-channel descriptor bit %d on, but file already closed",
        i + 1);  
      }
     err = TRUE;
     continue;
    }  
   __my_fclose(__mulchan_tab[i].mc_s);
   __mulchan_tab[i].mc_s = NULL;
   __my_free(__mulchan_tab[i].mc_fnam, strlen(__mulchan_tab[i].mc_fnam) + 1); 
   __mulchan_tab[i].mc_fnam = NULL;
  }
 if (!from_vpi)
  {
   if (((mcd >> 31) & 1L) != 0L)
    {
     __sgfwarn(611,
      "$fclose multi-channel descriptor bit 31 on, but no open file - unusable because reserved for new Verilog 2000 file I/O");
     err = TRUE;
    }
  }  

 if (err) return(bld_open_mcd());
 return(0);
}

/*
 * build a mc descriptor for open channels 
 */
static unsigned bld_open_mcd(void)
{ 
 unsigned mcd; 
 register int i;

 /* SJM 03/26/00 - high bit 32 reserved for new Verilog 2000 file I/O */
 for (i = 0, mcd = 0; i < 31; i++)
  {
   if (__mulchan_tab[i].mc_s == NULL) continue;
   mcd |= (1 << i);   
  }
 return(mcd);
}

/*
 * execute the multi-channel descriptor file open
 * assigns to next free descriptor slot if one available
 *
 * this is system function that returns 0 on fail 
 * 1 (index 0) is stdout and log file, 2 (index 1) is stder
 */
static int unsigned mc_do_fopen(struct expr_t *axp)
{
 int slen;
 char *chp;

 chp = __get_eval_cstr(axp, &slen);
 return(__mc1_fopen(chp, strlen(chp), FALSE));
}

/*
 * do the mcd fopen if possible
 */
extern unsigned __mc1_fopen(char *chp, int slen, int from_vpi)
{
 register int i; 
 FILE *tmp_s;

 /* SJm 03/26/00 - changed to match Verilog 2000 LRM */
 /* if name matches exactly return open - this is only heuristic */
 /* notice 2 is bit 3 (or value 4) that is first to use */
 /* bit 31 is rserved for new c style file open enhancement */
 for (i = 2; i < 31; i++) 
  {
   if (__mulchan_tab[i].mc_s == NULL) continue; 

   /* LOOKATME - not storing in tilde expanded form - same name can */
   /* mismatch but will just get open error from OS */
   if (strcmp(__mulchan_tab[i].mc_fnam, chp) == 0)
    {
     if (!from_vpi)
      {
       __sgfinform(433,
        "$fopen of %s failed: file already open and assigned to channel %d",
          chp, i + 1);
        __my_free(chp, slen + 1);
      }
     return((unsigned) (1L << i));
    }
  }

 for (i = 2; i < 31; i++) 
  { if (__mulchan_tab[i].mc_s == NULL) goto found_free; }
 if (!from_vpi)
  {
   __sgfinform(433,
    "$fopen of %s failed: no available multi-channel descriptor", chp);
  }
err_done:
 __my_free(chp, slen + 1);
 return(0);

found_free:
 if ((tmp_s = __tilde_fopen(chp, "w")) == NULL)
  {
   if (!from_vpi) 
    {
     __sgfinform(433, "$fopen of %s multi-channel bit %d failed: %s",
      chp, i, strerror(errno));
    }
   goto err_done;
  }
 __mulchan_tab[i].mc_s = tmp_s;
 /* know this is closed so no previous name to free */
 __mulchan_tab[i].mc_fnam = chp;
 /* notice first unused is 3 which is bit 4 on (if low bit is 1) */
 return((unsigned) (1L << i));
}


/*
 * execute showvars system task
 * this is called with fcall comma arg. list header not value 
 */
static void do_showvars_stask(struct expr_t *argxp)
{
 register int ni;
 register struct expr_t *xp;
 register struct net_t *np;
 int nd_itpop;
 struct gref_t *grp;
 struct task_t *tskp;
 struct expr_t *ndp;

 if (__iact_state) tskp = __scope_tskp;
 else tskp = __getcur_scope_tsk();

 /* notice here, calling itree location correct */
 if (argxp == NULL)
  {
   /* if no arguments - all variables in current scope */
   __cvsim_msg(">>> $showvars all local variables - scope %s type %s.\n",
    __msg_blditree(__xs, __inst_ptr, tskp), __inst_ptr->itip->imsym->synam);
   if (tskp != NULL)
    {
     if (tskp->trnum != 0)
      {
       np = &(tskp->tsk_regs[0]);
       for (ni = 0; ni < tskp->trnum; ni++, np++) __emit_1showvar(np, NULL);
      }
    }
   else
    {
     if (__inst_mod->mnnum != 0)
      {
       for (ni = 0, np = &(__inst_mod->mnets[0]); ni < __inst_mod->mnnum;
        ni++, np++)
        __emit_1showvar(np, NULL);
      }
    }
  }
 else
  {
   /* go through list of variables - may be xmr's */ 
   /* these can be only var, bit select or part select */
   __cvsim_msg(
    ">>> $showvars list of variables form - current scope %s type %s.\n",
    __msg_blditree(__xs, __inst_ptr, tskp), __inst_ptr->itip->imsym->synam);
   for (xp = argxp; xp != NULL; xp = xp->ru.x)
    {
     nd_itpop = FALSE;
     grp = NULL;
     ndp = xp->lu.x;
     if (ndp->optyp == LSB || ndp->optyp == PARTSEL)
      {
       np = ndp->lu.x->lu.sy->el.enp;
       if (ndp->lu.x->optyp == GLBREF)
        {
         grp = ndp->lu.x->ru.grp;
         __xmrpush_refgrp_to_targ(grp);
         nd_itpop = TRUE;
        }
      }
     else
      {
       if (ndp->optyp == GLBREF)
        { grp = ndp->ru.grp; __xmrpush_refgrp_to_targ(grp); nd_itpop = TRUE; }
       np = ndp->lu.sy->el.enp;
      }
     __emit_1showvar(np, grp);
     if (nd_itpop) __pop_itstk();
    }
  }
}

/*
 * set new suppressed warnings during simulation
 */
static void do_warn_supp_chg(char *stnam, struct expr_t *argxp, int supp)
{
 int argi;
 word ernum;
 struct expr_t *ndp;

 for (argi = 1; argxp != NULL; argxp = argxp->ru.x, argi++)
  {
   ndp = argxp->lu.x;
   if (!__get_eval_word(ndp, &ernum))
    {
bad_num:
     __sgferr(714,
      "%s argument %d value %s illegal or outside of inform/warning number range",
      stnam, argi, __msgexpr_tostr(__xs, ndp));
     continue;
    }
   if (!__enum_is_suppressable(ernum)) goto bad_num;

   if (supp) __wsupptab[ernum/WBITS] |= (1 << (ernum % WBITS));
   else __wsupptab[ernum/WBITS] &= ~(1 << (ernum % WBITS));
  }
}

/*
 * execute reset system task
 */
static void do_reset(struct expr_t *axp)
{
 int enter_iact, reset_val, diag_val;

 /* assume interactive entry in case reset value (2nd) arg missing */
 enter_iact = TRUE;
 reset_val = 0;
 diag_val = 0; 
 if (axp == NULL) goto do_it;
 if (get_opt_starg(axp, 0) != 0) enter_iact = FALSE;
 if ((axp = axp->ru.x) == NULL) goto do_it; 
 reset_val = get_opt_starg(axp, 0);
 if ((axp = axp->ru.x) == NULL) goto do_it; 
 diag_val = get_opt_starg(axp, 0);

do_it:
 if (diag_val >= 1)
  {
   if (!__quiet_msgs) __cv_msg("\n");
   __cv_msg("$reset to time 0 called from location %s at time %s.\n",
    __bld_lineloc(__xs2, (unsigned) __sfnam_ind, __slin_cnt),
    __to_timstr(__xs, &__simtime));
  }
 if (diag_val >= 2) __emit_stsk_endmsg();
 /* enter interactive unless reset value given and non zero */
 if (reset_val != 0) enter_iact = FALSE;

 if (enter_iact) __stop_before_sim = TRUE;
 else __stop_before_sim = FALSE;
 /* record state changes caused by arguments */
 __reset_value = reset_val;

 /* reenable the normal ^c signal handler - when variables reset */
 /* sim will replace with sim handler for entering interactive */
#if defined(INTSIGS)
 signal(SIGINT, __comp_sigint_handler);
#else
 signal(SIGINT, (void (*)()) __comp_sigint_handler);
#endif

 /* this does not return - uses lng jmp */
 longjmp(__reset_jmpbuf, 1);
}

/*
 * write the scope information
 * uses current scope not interactive - except $scope also changes current 
 * if in interactive mode
 */
static void do_showscopes(struct expr_t *axp)
{
 word flag;
 struct task_t *tskp;
 struct mod_t *imdp;
 struct sy_t *syp;

 if (axp == NULL) flag = 0;
 else if (!__get_eval_word(axp->lu.x, &flag))
  {
   __sgfwarn(646, "$showscopes argument value %s has x/z bits - made 0",
    __msgexpr_tostr(__xs, axp->lu.x));
   flag = 0;
  }
 /* use current thread to determine if in task */
 /* if no thread (sim. not started) cannot be active task */
 if (__cur_thd == NULL) tskp = NULL; else tskp = __getcur_scope_tsk();

 /* 0 means current level, other value all underneath */
 if (flag == 0)
  {
   if (tskp == NULL) prt_1m_scopelist(__inst_ptr);
   else prt_1tsk_scopelist(tskp, TRUE);
  }
 else
  { 
   __cvsim_msg("Nested scopes:\n");
   __outlinpos = 0;
   __pv_stlevel = 0;

   if (tskp == NULL) prt_1m_nestscopes(__inst_ptr);
   /* if current scope is task, must print out named blocks inside */
   else prt_1tsk_nestscopes(tskp->tsksymtab->sytofs);

   __pv_stlevel = 0;
   __outlinpos = 0;
  }

 /* final step is printing current scope and list of top level modules */
 imdp = __inst_mod;
 
 if (tskp == NULL) syp = imdp->msym; else syp = tskp->tsksyp;
 __cvsim_msg("Current scope: %s (file %s line %d)\n",
  __msg_blditree(__xs, __inst_ptr, tskp), __schop(__xs2,
  __in_fils[syp->syfnam_ind]), syp->sylin_cnt); 
 __prt_top_mods();
}

/*
 * show one module scope level given itree location 
 *
 * notice this is sort of static since once at itree location under same
 * 4 catagories of scopes: instances, tasks, functions, named blocks
 * everything here but named blockes in tasks/functions or named blocks
 */
static void prt_1m_scopelist(struct itree_t *itp)
{
 register int i;
 register struct task_t *tskp;
 int none, first_time;
 struct mod_t *imdp;
 struct inst_t *ip;

 if (__outlinpos != 0) __misc_terr(__FILE__, __LINE__);
 /* first instances */
 __pv_stlevel = 0;
 imdp = itp->itip->imsym->el.emdp;
 if (imdp->minum != 0) __wrap_puts("  Instances:", stdout);
 __pv_stlevel = 3;
 for (i = 0; i < imdp->minum; i++)
  {
   ip = &(imdp->minsts[i]);
   __wrap_putc(' ', stdout);
   __wrap_puts(ip->isym->synam, stdout);
   __wrap_putc('(', stdout);
   __wrap_puts(ip->imsym->synam, stdout);
   if (i < imdp->minum - 1) __wrap_puts("),", stdout);
   else __wrap_putc(')', stdout);
  }
 if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
 __pv_stlevel = 0;

 /* next tasks */
 for (none = TRUE, tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
  { if (tskp->tsktyp == TASK) { none = FALSE; break; } } 
 if (!none) 
  {
   __wrap_puts("  Tasks:", stdout);
   __pv_stlevel = 3;
   first_time = TRUE;
   for (tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
    {
     if (tskp->tsktyp != TASK) continue;
     if (first_time) { __wrap_putc(' ', stdout); first_time = FALSE; }
     else __wrap_puts(", ", stdout);
     __wrap_puts(tskp->tsksyp->synam, stdout);
    }
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   __pv_stlevel = 0;
  }
 /* next functons */
 for (none = TRUE, tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
  { if (tskp->tsktyp == FUNCTION) { none = FALSE; break; } } 
 if (!none) 
  {
   __wrap_puts("  Functions:", stdout);
   __pv_stlevel = 3;
   first_time = TRUE;
   for (tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
    {
     if (tskp->tsktyp != FUNCTION) continue;
     if (first_time) { __wrap_putc(' ', stdout); first_time = FALSE; }
     else __wrap_puts(", ", stdout);
     __wrap_puts(tskp->tsksyp->synam, stdout);
    }
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   __pv_stlevel = 0;
  }
 /* finally named blocks */
 for (none = TRUE, tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
  {

   if (tskp->tsktyp == FORK || tskp->tsktyp == Begin)
    { none = FALSE; break; }
  } 
 if (!none) 
  {
   __wrap_puts("  Named blocked:", stdout);
   __pv_stlevel = 3;
   first_time = TRUE;
   for (tskp = imdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
    {

     if (tskp->tsktyp == TASK || tskp->tsktyp == FUNCTION) continue;
     if (first_time) { __wrap_putc(' ', stdout); first_time = FALSE; }
     else __wrap_puts(", ", stdout);
     __wrap_puts(tskp->tsksyp->synam, stdout);
    }
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   __pv_stlevel = 0;
  }
}

/*
 * print the scopes in a task scope
 * here can only be named blocks located from symbol table
 * separate routine for recursive named block listing
 */
static void prt_1tsk_scopelist(struct task_t *tskp, int nd_msg)
{
 register struct symtab_t *sytp2;
 struct symtab_t *sytp;
 int first_time;
 
 sytp = tskp->tsksymtab;
 if (sytp->sytofs == NULL && nd_msg) return;
 __wrap_puts("  Named blocks:", stdout);
 __pv_stlevel = 3;
 first_time = FALSE;
 for (sytp2 = sytp->sytofs; sytp2 != NULL; sytp2 = sytp2->sytsib)
  {
   if (first_time) { first_time = FALSE; __wrap_putc(' ', stdout); }
   else __wrap_puts(", ", stdout);
   __wrap_puts(sytp2->sypofsyt->synam, stdout);
  }
 if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
 __pv_stlevel = 0;
 if (__outlinpos != 0) __misc_terr(__FILE__, __LINE__);
 /* first instances */
 __pv_stlevel = 0;
}

/*
 * for module print nested scopes with indent - to show the scope structure
 */
static void prt_1m_nestscopes(struct itree_t *itp)
{
 register int i;
 register struct task_t *tskp;
 struct mod_t *mdp;
 struct itree_t *down_itp;
 struct inst_t *ip;

 __pv_stlevel++;
 mdp = itp->itip->imsym->el.emdp;
 for (i = 0; i < mdp->minum; i++)
  {
   down_itp = &(itp->in_its[i]);
   ip = down_itp->itip;
   __wrap_putc(' ', stdout);
   __wrap_puts(ip->isym->synam, stdout);
   __wrap_putc('(', stdout);
   __wrap_puts(ip->imsym->synam, stdout);
   __wrap_putc(')', stdout);
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   prt_1m_nestscopes(down_itp);
  }
 /* next tasks */
 __pv_stlevel++;
 for (tskp = mdp->mtasks; tskp != NULL; tskp = tskp->tsknxt)
  {

   sprintf(__xs, "%s: ", __to_tsktyp(__xs2, tskp->tsktyp));
   __wrap_puts(__xs, stdout);
   __wrap_puts(tskp->tsksyp->synam, stdout);
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   if (tskp->tsksymtab->sytofs != NULL)
    {
     __pv_stlevel++;
     prt_1tsk_nestscopes(tskp->tsksymtab->sytofs);
     __pv_stlevel--;
    }
  }
 __pv_stlevel -= 2;
}

static void prt_1tsk_nestscopes(struct symtab_t *up_sytp)
{
 struct symtab_t *sytp; 
 struct task_t *tskp;

 for (sytp = up_sytp->sytofs; sytp != NULL; sytp = sytp->sytsib) 
  {
   tskp = sytp->sypofsyt->el.etskp;
   sprintf(__xs, "%s: ", __to_tsktyp(__xs2, tskp->tsktyp));
   __wrap_puts(__xs, stdout);
   __wrap_puts(tskp->tsksyp->synam, stdout);
   if (__outlinpos != 0) { __wrap_putc('\n', stdout); __outlinpos = 0; }
   if (tskp->tsksymtab->sytofs != NULL)
    {
     __pv_stlevel++;
     prt_1tsk_nestscopes(tskp->tsksymtab->sytofs);
     __pv_stlevel--;
    }
  }
}

/*
 * BUILT INTO VERILOG STOCHASTIC QUEUE SYSTEM TASKS
 */

/*
 * execute the qfull function - must push 1 (room), 0 no room onto xstk
 *
 * LOOKATME - is this a 1 bit 0/1?
 */
static void exec_qfull(struct expr_t *argxp)
{
 int q_id, rv;
 word val;
 struct q_hdr_t *q_p;
 struct expr_t *xp, *a1xp, *a2xp;
 struct xstk_t *xsp;

 rv = 0;
 /* access the required 4 arguments */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 /* first element in function arg. list is return variable */
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a1xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a2xp = xp->lu.x;

 if (!__get_eval_word(a1xp, &val))
  {
   __sgfwarn(596, "$q_full argument 1, q_id value %s x/z or out of range",
    __msgexpr_tostr(__xs, a1xp));
ret_x:
   push_xstk_(xsp, 1);
   xsp->ap[0] = 1;
   xsp->bp[0] = 1;
   rv = 2;
   goto done;
  }
 q_id = (int) val;

 /* find q that matches passed q id */
 if ((q_p = find_q_from_id(q_id)) == NULL) goto ret_x;

 push_xstk_(xsp, 1);
 xsp->bp[0] = 0;
 if (q_p->q_size >= q_p->q_maxlen) xsp->ap[0] = 1;
 else xsp->ap[0] = 0;

done:
 if (a2xp->optyp == OPEMPTY) return;

 push_xstk_(xsp, WBITS);
 xsp->ap[0] = (word) rv; 
 xsp->bp[0] = 0L;
 if (xsp->xslen != a2xp->szu.xclen) __sizchgxs(xsp, a2xp->szu.xclen);
 __exec2_proc_assign(a2xp, xsp->ap, xsp->bp);
 __pop_xstk();
}

/*
 * initialize a queue
 *
 * know exactly 4 args (possibly ,,) or will not get here
 */
static void do_q_init(struct expr_t *argxp)
{
 int q_id, q_type, q_maxlen, rv;
 word val;
 struct q_hdr_t *q_p;
 struct expr_t *xp, *a1xp, *a2xp, *a3xp, *a4xp;
 struct xstk_t *xsp;

 rv = 0;
 /* access the required 4 arguments */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 a1xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a2xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a3xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a4xp = xp->lu.x;
 if (xp->ru.x != NULL) __arg_terr(__FILE__, __LINE__);

 /* access the rhs arguments */
 if (!__get_eval_word(a1xp, &val))
  {
   __sgfwarn(596, "$q_initialize argument 1, q_id value %s x/z or out of range",
    __msgexpr_tostr(__xs, a1xp));
   rv = 2;
   q_id = 0;
  }
 else q_id = (int) val;
 if (!__get_eval_word(a2xp, &val) || val < 1 || val > 2)
  {
   __sgfwarn(596,
    "$q_initialize argument 2, q_type value %s x/z or out of range",
    __msgexpr_tostr(__xs, a2xp));
   if (rv == 0) rv = 4;
   q_type = 0;
  }
 else q_type = (int) val;
 if (!__get_eval_word(a3xp, &val)) 
  {
bad_qlen:
   __sgfwarn(596,
    "$q_initialize argument 3, max_length value %s x/z or negative",
    __msgexpr_tostr(__xs, a3xp));
   if (rv == 0) rv = 5;
   q_maxlen = 0;
  }
 else
  {
   q_maxlen = (int) val;
   if (q_maxlen <= 0) goto bad_qlen;
  }
 if (rv != 0) goto done;

 /* make sure id is unqiue */
 if (find_q_from_id(q_id) != NULL) { rv = 6; goto done; }

 /* allocate the new q header and link into q list */
 q_p = (struct q_hdr_t *) __my_malloc(sizeof(struct q_hdr_t));
 init_q(q_p);
 if (__qlist_hdr == NULL) __qlist_hdr = q_p; 
 else { q_p->qhdrnxt = __qlist_hdr; __qlist_hdr = q_p; }
 if (q_type == 1) q_p->q_fifo = TRUE; else q_p->q_fifo = FALSE;
 q_p->qarr = (struct q_val_t *) __my_malloc(q_maxlen*sizeof(struct q_val_t)); 
 memset(q_p->qarr, 0, q_maxlen*sizeof(struct q_val_t));
 q_p->q_id = q_id;
 q_p->q_maxlen = q_maxlen;

done:
 if (a4xp->optyp == OPEMPTY) return;

 push_xstk_(xsp, WBITS);
 xsp->ap[0] = (word) rv; 
 xsp->bp[0] = 0L;
 if (xsp->xslen != a4xp->szu.xclen) __sizchgxs(xsp, a4xp->szu.xclen);
 __exec2_proc_assign(a4xp, xsp->ap, xsp->bp);
 __pop_xstk();
}

/*
 * initialize a q
 */
static void init_q(struct q_hdr_t *q_p)
{
 q_p->q_fifo = FALSE;
 q_p->q_id = 0;
 q_p->q_hdr = -1;
 q_p->q_tail = -1;
 q_p->q_maxlen = 0;
 q_p->q_size = 0;
 q_p->q_minwait = 0xffffffffffffffffULL;
 q_p->qhdrnxt = NULL;
}

/*
 * find q header record from identifying q id number
 *
 * LOOKATME - could use binary search but think will not be many queues
 */
static struct q_hdr_t *find_q_from_id(int id)
{
 register struct q_hdr_t *qp;

 for (qp = __qlist_hdr; qp != NULL; qp = qp->qhdrnxt)
  {
   if (qp->q_id == id) return(qp);
  }
 return(NULL);
}

/*
 * add an element to a queue
 *
 * know exactly 4 args (possibly ,,) or will not get here
 */
static void do_q_add(struct expr_t *argxp)
{
 int q_id, qjob_id, qinform_id, rv;
 word val;
 struct q_hdr_t *q_p;
 struct q_val_t *qvp;
 struct expr_t *xp, *a1xp, *a2xp, *a3xp, *a4xp;
 struct xstk_t *xsp;

 rv = 0;
 /* access the required 4 arguments */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 a1xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a2xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a3xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a4xp = xp->lu.x;
 if (xp->ru.x != NULL) __arg_terr(__FILE__, __LINE__);

 /* access the rhs arguments */
 if (!__get_eval_word(a1xp, &val))
  {
   __sgfwarn(596, "$q_add argument 1, q_id value %s x/z or out of range",
    __msgexpr_tostr(__xs, a1xp));
   rv = 2;
   q_id = 0;
  }
 else q_id = (int) val;
 if (a2xp->optyp == OPEMPTY) qjob_id = 0;
 else
  {
   if (!__get_eval_word(a2xp, &val))
    {
     __sgfwarn(596,
      "$q_add argument 2, job_id value %s x/z or too wide (0 used)",
      __msgexpr_tostr(__xs, a2xp));
     val = 0;
    }
   qjob_id = (int) val;
  }
 if (a3xp->optyp == OPEMPTY) qinform_id = 0;
 else
  {
   if (!__get_eval_word(a3xp, &val)) 
    {
     __sgfwarn(596,
      "$q_add argument 3, inform_id value %s x/z or too wide (0 used)",
      __msgexpr_tostr(__xs, a3xp));
     val = 0;
    }
   qinform_id = (int) val;
  }
 if (rv != 0) goto done;

 /* find q that matches passed q id */
 if ((q_p = find_q_from_id(q_id)) == NULL) { rv = 2; goto done; }

 /* add the element */
 if (q_p->q_fifo)
  {
   if (q_p->q_hdr == -1) q_p->q_hdr = q_p->q_tail = 0;
   else
    {
     if (q_p->q_size >= q_p->q_maxlen) { rv = 1; goto done; }
     (q_p->q_hdr)++;
     /* wrap queue around - since size not too big know empty */ 
     if (q_p->q_hdr >= q_p->q_maxlen) q_p->q_hdr = 0;
    }
  }
 else
  {
   /* easy stack lifo case - q tail not used */
   if (q_p->q_hdr == -1) q_p->q_hdr = 0;
   else
    {
     if (q_p->q_size >= q_p->q_maxlen) { rv = 1; goto done; }
     (q_p->q_hdr)++;
    }
  }
 qvp = &(q_p->qarr[q_p->q_hdr]);
 (q_p->q_size)++;
 if (q_p->q_size > q_p->q_maxsize) q_p->q_maxsize = q_p->q_size;
 qvp->job_id = qjob_id;
 qvp->inform_id = qinform_id;
 qvp->enter_tim = __simtime;

done:
 if (a4xp->optyp == OPEMPTY) return;

 push_xstk_(xsp, WBITS);
 xsp->ap[0] = (word) rv; 
 xsp->bp[0] = 0L;
 if (xsp->xslen != a4xp->szu.xclen) __sizchgxs(xsp, a4xp->szu.xclen);
 __exec2_proc_assign(a4xp, xsp->ap, xsp->bp);
 __pop_xstk();
}

/*
 * delete an element from a queue
 *
 * know exactly 4 args (possibly ,,) or will not get here
 */
static void do_q_remove(struct expr_t *argxp)
{
 int q_id, qjob_id, qinform_id, rv;
 word val;
 word64 timval;
 struct q_hdr_t *q_p;
 struct q_val_t *qvp;
 struct expr_t *xp, *a1xp, *a2xp, *a3xp, *a4xp;
 struct xstk_t *xsp;

 rv = 0;
 /* access the required 4 arguments - last 3 outputs can be empty */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 a1xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a2xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a3xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a4xp = xp->lu.x;
 if (xp->ru.x != NULL) __arg_terr(__FILE__, __LINE__);

 /* access the rhs arguments */
 if (!__get_eval_word(a1xp, &val))
  {
   __sgfwarn(596, "$q_remove argument 1, q_id value %s x/z or out of range",
    __msgexpr_tostr(__xs, a1xp));
   rv = 2;
   goto done;
  }
 else q_id = (int) val;

 /* find q that matches passed q id */
 if ((q_p = find_q_from_id(q_id)) == NULL) { rv = 2; goto done; }

 /* here no assignment to output values */
 if (q_p->q_size == 0) { rv = 3; goto done; }

 qvp = &(q_p->qarr[q_p->q_tail]); 
 /* delete the element - take off from tail */
 if (q_p->q_fifo)
  {
   if (q_p->q_size == 1) q_p->q_hdr = q_p->q_tail = -1;
   else
    {
     (q_p->q_tail)++;
     /* wrap queue around - since size not too big know empty */ 
     if (q_p->q_tail >= q_p->q_maxlen) q_p->q_tail = 0;
    }
  }
 /* easy stack lifo case - q tail not used */
 else (q_p->q_hdr)--;

 /* save minimum time in q (wait time) */
 timval = __simtime - qvp->enter_tim;
 if (timval < q_p->q_minwait) q_p->q_minwait = timval;

 (q_p->q_size)--;
 qjob_id = qvp->job_id;
 qinform_id = qvp->inform_id;

 if (a2xp->optyp != OPEMPTY)
  {
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) qjob_id; 
   xsp->bp[0] = 0L;
   if (xsp->xslen != a2xp->szu.xclen) __sizchgxs(xsp, a2xp->szu.xclen);
   __exec2_proc_assign(a2xp, xsp->ap, xsp->bp);
   __pop_xstk();
  }

 if (a3xp->optyp != OPEMPTY)
  {
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) qinform_id; 
   xsp->bp[0] = 0L;
   if (xsp->xslen != a3xp->szu.xclen) __sizchgxs(xsp, a3xp->szu.xclen);
   __exec2_proc_assign(a3xp, xsp->ap, xsp->bp);
   __pop_xstk();
  }

done:
 if (a4xp->optyp == OPEMPTY) return;

 push_xstk_(xsp, WBITS);
 xsp->ap[0] = (word) rv; 
 xsp->bp[0] = 0L;
 if (xsp->xslen != a4xp->szu.xclen) __sizchgxs(xsp, a4xp->szu.xclen);
 __exec2_proc_assign(a4xp, xsp->ap, xsp->bp);
 __pop_xstk();
}

/*
 * examine a queue
 *
 * know exactly 4 args (possibly ,,) or will not get here
 */
static void do_q_examine(struct expr_t *argxp)
{
 int q_id, q_stat_code, rv;
 word val;
 word64 timval;
 struct q_hdr_t *q_p;
 struct expr_t *xp, *a1xp, *a2xp, *a3xp, *a4xp;
 struct xstk_t *xsp;

 rv = 0;
 /* access the required 4 arguments - last 3 outputs can be empty */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 a1xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a2xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a3xp = xp->lu.x;
 if ((xp = xp->ru.x) == NULL) __arg_terr(__FILE__, __LINE__);
 a4xp = xp->lu.x;
 if (xp->ru.x != NULL) __arg_terr(__FILE__, __LINE__);

 /* access the rhs arguments */
 if (!__get_eval_word(a1xp, &val))
  {
   __sgfwarn(596, "$q_examine argument 1, q_id value %s x/z or out of range",
    __msgexpr_tostr(__xs, a1xp));
   rv = 2;
   goto done;
  }
 q_id = (int) val;

 if (!__get_eval_word(a2xp, &val) || val < 1 || val > 6)
  {
   __sgfwarn(596,
    "$q_examine argument 2, q_stat_code value %s x/z or out of range",
    __msgexpr_tostr(__xs, a2xp));
   /* LOOKATME - really no good value for this error */
   q_stat_code = 0;
   rv = 4;
  }
 else q_stat_code = (int) val;

 if (rv != 0) goto done;

 /* find q that matches passed q id */
 if ((q_p = find_q_from_id(q_id)) == NULL) { rv = 2; goto done; }

 switch (q_stat_code) {
  case 1:
   /* current size of q */
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) q_p->q_size;
   xsp->bp[0] = 0L;
   break;
  case 2:
   /* mean inter arrival time for elements currently in the q */
   cmp_mean_interarriv_tim(&timval, q_p);
push_cmp_tim:
   push_xstk_(xsp, 64);
   xsp->ap[0] = (word) (timval & WORDMASK_ULL);
   xsp->ap[1] = (word) ((timval >> 32) & WORDMASK_ULL); 
   xsp->bp[0] = xsp->bp[1] = 0L;
   break;
  case 3:
   /* maximum size queue ever attained */
   push_xstk_(xsp, WBITS);
   xsp->ap[0] = (word) q_p->q_maxsize;
   xsp->bp[0] = 0L;
   break;
  case 4:
   push_xstk_(xsp, 64);
   xsp->ap[0] = (word) (q_p->q_minwait & WORDMASK_ULL);
   xsp->ap[1] = (word) ((q_p->q_minwait >> 32) & WORDMASK_ULL);
   xsp->bp[0] = xsp->bp[1] = 0L;
   break;
  case 5:
   cmp_max_wait(&timval, q_p);
   goto push_cmp_tim;
  case 6:
   cmp_mean_wait_tim(&timval, q_p);
   goto push_cmp_tim;
  default: __case_terr(__FILE__, __LINE__); return;
 }

 /* only assign result, lhs arg. passed */
 if (a3xp->optyp != OPEMPTY)
  {
   if (xsp->xslen != a3xp->szu.xclen) __sizchgxs(xsp, a3xp->szu.xclen);
   __exec2_proc_assign(a3xp, xsp->ap, xsp->bp);
  }
 __pop_xstk();

done:
 if (a4xp->optyp == OPEMPTY) return;

 push_xstk_(xsp, WBITS);
 xsp->ap[0] = (word) rv; 
 xsp->bp[0] = 0L;
 if (xsp->xslen != a4xp->szu.xclen) __sizchgxs(xsp, a4xp->szu.xclen);
 __exec2_proc_assign(a4xp, xsp->ap, xsp->bp);
 __pop_xstk();
}

/*
 * compute average inter (between 2) arrival time for qs currently in queue
 */
static void cmp_mean_interarriv_tim(word64 *timvalp, struct q_hdr_t *q_p)
{
 register int qi, i;
 word64 avgtim, arrdif, quot, rem;

 /* for one or less size q mean always 0 */
 if (q_p->q_size <= 1) { *timvalp = 0ULL; return; }
 avgtim = 0ULL;
 if (q_p->q_fifo)
  {
   for (qi = q_p->q_hdr, i = 0; i < q_p->q_size; i++)
    {
     if ((++qi) >= q_p->q_size) qi = 0;
     if (i == 0) continue;

     arrdif = q_p->qarr[qi].enter_tim - q_p->qarr[qi - 1].enter_tim;
     avgtim += arrdif;
    }
  }
 else
  {
   /* easy lifo stack case */
   for (qi = 0; qi < q_p->q_size; qi++)
    {
     if (qi == 0) continue;
     arrdif = q_p->qarr[qi].enter_tim - q_p->qarr[qi - 1].enter_tim;
     avgtim += arrdif;
    }
  }
 /* divide - round - know q at least 2 elements to get here */
 /* SJM 02/03/00 - cast of negative (>2**31) sign extends need word 1st */
 quot = avgtim/((word64) (((word) q_p->q_size) - 1)); 
 rem = avgtim % ((word64) (((word) q_p->q_size) - 1));
 avgtim = quot;
 if (rem >= ((word64) (((word) q_p->q_size)/2))) avgtim++;
 *timvalp = avgtim;
}

/*
 * compute longest wait (in queue) time for elements in queue
 */
static void cmp_max_wait(word64 *timvalp, struct q_hdr_t *q_p)
{
 register int qi, i;
 word64 inqtim;

 if (q_p->q_size <= 1) { *timvalp = 0ULL; return; }
 if (q_p->q_fifo)
  {
   for (qi = q_p->q_hdr, i = 0; i < q_p->q_size; i++)
    {
     if ((++qi) >= q_p->q_size) qi = 0;
     if (i == 0)
      {
       *timvalp = q_p->qarr[qi].enter_tim;
       continue;
      }
     inqtim = __simtime - q_p->qarr[qi].enter_tim;
     if (inqtim < *timvalp) *timvalp = inqtim;
    }
  }
 else
  {
   /* easy lifo stack case */
   for (qi = 0; qi < q_p->q_size; qi++)
    {
     if (qi == 0)
      {
       *timvalp = q_p->qarr[qi].enter_tim;
       continue;
      }
     inqtim = __simtime - q_p->qarr[qi].enter_tim;
     if (inqtim < *timvalp) *timvalp = inqtim;
    }
  }
}

/*
 * compute average (mean) time each element has spent in queue
 */
static void cmp_mean_wait_tim(word64 *timvalp, struct q_hdr_t *q_p)
{
 register int qi, i;
 word64 avgtim, waitdif, quot, rem;

 if (q_p->q_size <= 0) { *timvalp = 0ULL; return; }
 avgtim = 0ULL;
 if (q_p->q_fifo)
  {
   for (qi = q_p->q_hdr, i = 0; i < q_p->q_size; i++)
    {
     if ((++qi) >= q_p->q_size) qi = 0;
     waitdif = __simtime - q_p->qarr[qi].enter_tim; 
     avgtim += waitdif;
    }
  }
 else
  {
   /* easy lifo stack case */
   for (qi = 0; qi < q_p->q_size; qi++)
    {
     waitdif = __simtime - q_p->qarr[qi].enter_tim;
     avgtim += waitdif;
    }
  }
 /* divide - round - know q at least 1 element to get here */
 quot = avgtim/((word64) ((word) q_p->q_size)); 
 rem = avgtim % ((word64) ((word) q_p->q_size));
 avgtim = quot;
 if (rem >= ((word64) (((word) q_p->q_size)/2))) avgtim++;
 *timvalp = avgtim;
}

/*
 * TIMESCALE TASK ROUTINES 
 */

/*
 * execute the print time scale
 * know the 1 arg. is xmr or no arg means current scope
 *
 * know arg. is xmr even if only has one component
 * axp is nil (for none) or function call comma operator (head of list)
 */
static void exec_prttimscale(struct expr_t *axp)
{
 struct mod_t *mdp;
 struct expr_t *ndp; 
 char s1[RECLEN], s2[RECLEN], s3[RECLEN];

 __cur_sofs = 0;
 __adds("Time scale of (");
 if (axp == NULL)
  {
   mdp = __scope_ptr->itip->imsym->el.emdp;
   __disp_itree_path(__inst_ptr, (struct task_t *) NULL);
  }
 else 
  {
   ndp = axp->lu.x;
   if (ndp->optyp == GLBREF)
    { mdp = ndp->ru.grp->targmdp; __adds(ndp->ru.grp->gnam); }
   else if (ndp->lu.sy->sytyp == SYM_M)
    { mdp = ndp->lu.sy->el.emdp; __adds(mdp->msym->synam); }
   else { __case_terr(__FILE__, __LINE__);  return; }
  }
 sprintf(s1, ") is %s / %s\n",
  __to_timunitnam(s2, mdp->mtime_units),
  __to_timunitnam(s3, mdp->mtime_units + mdp->mtime_prec));
 __adds(s1);
 /* notice cannot truncate here */
 my_puts_(__exprline, stdout);
 __cur_sofs = 0;
}

/*
 * execute the time format system task - just sets some internal values 
 * know exactly four arguments or will not get here
 *
 * this allows ,, and missing arguments that mean use 0 - error
 */
static void exec_timefmt(struct expr_t *argxp) 
{
 int argi, i1, slen;
 word val, val1, val2, val3;
 struct expr_t *xp, *ndp;
 char *chp;

 /* set values for missing arguments */
 val1 = val2 = val3 = 0; 

 argi = 1;
 /* empty arg. list already checked for */
 if ((xp = argxp) == NULL) __arg_terr(__FILE__, __LINE__);
 ndp = xp->lu.x;
 if (ndp->optyp != OPEMPTY)
  {
   /* eval. here is just word stored in 2 complement */
   if (!__get_eval_word(ndp, &val)) goto bad_arg; 
   i1 = (int) val;
   if (i1 > 0 || i1 < -15) goto bad_arg;
   i1 = -i1;
   val1 = (word) i1;
   if (val1 > __des_timeprec)
    { 
     __sgferr(1240,
      "$timeformat units %s (%d) impossible - must be larger than %s (%d) tick",
       __to_timunitnam(__xs, (unsigned) val1), -((int) val1),
       __to_timunitnam(__xs2, (unsigned) __des_timeprec),
       -((int) __des_timeprec));
     /* change nothing if this does not change */
     return;
    }
  }
 if ((xp = xp->ru.x) == NULL)
  { chp = __my_malloc(1); *chp = '\0'; slen = 1; goto do_chg; }

 ndp = xp->lu.x; 
 argi++;
 if (ndp->optyp != OPEMPTY)
  {
   if (!__get_eval_word(ndp, &val)) goto bad_arg;
   i1 = (int) val;
   if (i1 < 0 || i1 >= RECLEN) goto bad_arg;
   val2 = val;
  }

 if ((xp = xp->ru.x) == NULL)
  { chp = __my_malloc(1); *chp = '\0'; slen = 1; goto do_chg; }
 ndp = xp->lu.x;
 argi++;
 if (ndp->optyp != OPEMPTY)
  {
   chp = __get_eval_cstr(ndp, &slen);
   /* must fit in RECLEN style string - but maybe should be narrower */ 
   if (slen >= RECLEN) { __my_free(chp, slen + 1); goto bad_arg; }
  }
 else { chp = __my_malloc(1); *chp = '\0'; slen = 1; }
 
 if ((xp = xp->ru.x) == NULL) goto do_chg;
 ndp = xp->lu.x;
 argi++;
 /* must allow ,) form */
 if (ndp->optyp != OPEMPTY)
  {
   if (!__get_eval_word(ndp, &val) || val > 40)
    {
     __sgferr(1047,
      "$timeformat minimum field width must be between 0 and 40 - not changed");
     __my_free(chp, slen + 1);
     return;
    }
   val3 = val;
  }
 
do_chg:
 if (slen > (int) (val3 + 1))  
  {
   __sgferr(1047,
    "$timeformat suffix length %d wider than minimum field width (%d) - not changed",
    slen, val3);
   __my_free(chp, slen + 1);
   return;
  }
 __tfmt_units = val1;
 __tfmt_precunits = val2;
 __my_free((char *) __tfmt_suf, strlen(__tfmt_suf) + 1);
 __tfmt_suf = chp;
 __tfmt_minfwid = val3;
 return;

bad_arg:
 __sgferr(713, "$timeformat argument %d value %s x/z or out of range",
  argi, __msgexpr_tostr(__xs, ndp));
}

/*
 * get an optional system task control argument
 * gets converted to machine int
 * this must be called with xp head of fcall list (comma operator)
 */
static int get_opt_starg(struct expr_t *xp, int dflt_val)
{
 int val;
 word rval;
 struct expr_t *axp;

 if (xp == NULL) return(dflt_val);
 axp = xp->lu.x;
 if (axp->optyp == OPEMPTY) return(dflt_val);

 if (!__get_eval_word(axp, &rval))
  {
   __sgfwarn(519,
    "optional system task numeric argument has x/z bits - default used");
   return(dflt_val);
  }
 val = (int) rval;
 return(val); 
}

/*
 * evaluate a value to an int (return F if not a non x/z WBIT int)
 * this must be called with actual argument expr. not fcall comma expr.
 */
extern int __get_eval_word(struct expr_t *xp, word *wval)
{
 int rval;
 struct xstk_t *xsp;

 *wval = 0;
 xsp = __eval_xpr(xp); 
 /* semantics says there is an implied conversion from real to int */
 /* but not across system task/func. arguments */
 /* however this routine is only called when int needed */
 if (xp->optyp == REALNUM || xp->optyp == ISREALNUM)
  {
   double d1;

   /* truncating since for getting 32 bit value */
   memcpy(&d1, xsp->ap, sizeof(double));
   *wval = (word) d1; 
   rval = TRUE; 
   goto done;
  }
 if (xsp->xslen > WBITS)
  {
   if (!vval_is0_(&(xsp->ap[1]), xsp->xslen - WBITS)
    || !vval_is0_(&(xsp->bp[1]), xsp->xslen - WBITS))
    { rval = FALSE; goto done; }
  }
 if (xsp->bp[0] != 0L) { rval = FALSE; goto done; }
 *wval = xsp->ap[0];
 rval = TRUE;

done:
 __pop_xstk();
 return(rval);
}

/*
 * exec the $log file system task
 * this is called with fcall comma operator header
 */
static void exec_log_fnamchg(struct expr_t *axp)
{ 
 int slen;
 FILE *tmp_s;
 char *chp;

 if (axp == NULL) { __log_s = __save_log_s; return; }
 chp = __get_eval_cstr(axp->lu.x, &slen);
 if ((tmp_s = __tilde_fopen(chp, "w")) == NULL)
  {
   __sgferr(1243, 
    "cannot open new $log output transcript file %s - not changed",
    __exprline); 
   __my_free(chp, slen + 1);
   return;
  }
 if (__log_s != NULL && __log_s != stdout && __log_s != stderr)
  {
   __my_fclose(__log_s);
   __my_free(__log_fnam, strlen(__log_fnam) + 1);
   __log_fnam = NULL;
  }
 __log_fnam = chp; 
 __log_s = tmp_s;
 __save_log_s = NULL;
 /* SJM 03/26/00 - log file now not an mcd - lumped with 0 (stdout) */
 /* ---
 __mulchan_tab[2].mc_fnam = __pv_stralloc(__log_fnam);
 __mulchan_tab[2].mc_fnam = __pv_stralloc(__log_fnam);
 --- */ 
 if (__verbose)
  __cv_msg("  Now writing output log to file \"%s\".\n", __log_fnam);
}

/*
 * exec the $tracefile system task
 * always open - if no writing into then just empty file
 *
 * will not get here if name argument missing 
 * this is called with actual file name argument not fcall list header op
 * 
 */
static void exec_trace_fnamchg(struct expr_t *argvx)
{ 
 int slen;
 FILE *tmp_s;
 char *chp;

 chp = __get_eval_cstr(argvx, &slen);
 if (strcmp(chp, "STDOUT") == 0) strcpy(chp, "stdout");
 if (strcmp(__tr_fnam, chp) == 0) 
  {
   __sgfwarn(625, "$tracefile file name %s same as previous - task ignored",
    __tr_fnam);
   goto done;
  }
 /* if changing to stdout set it, but cannot open */
 if (strcmp(chp, "stdout") == 0)
  {
   if (__tr_s != NULL && __tr_s != stdout && __tr_s != stderr)
    __my_fclose(__tr_s);

   if (__tr_fnam != NULL) __my_free(__tr_fnam, strlen(__tr_fnam) + 1);
   __tr_fnam = chp;
   __tr_s = stdout;
   goto new_tr;
  }
 /* know new file not stdout - always open system task new trace file */
 if ((tmp_s = __tilde_fopen(chp, "w")) == NULL)
  {
   __sgferr(1247, "cannot open new trace output file %s - not changed",
    chp); 
   goto done;
  }
 if (__tr_s != NULL && __tr_s != stdout && __tr_s != stderr)
  __my_fclose(__tr_s);

 if (__tr_fnam != NULL) __my_free(__tr_fnam, strlen(__tr_fnam) + 1);
 __tr_fnam = chp;
 __tr_s = tmp_s;

new_tr:
 if (__verbose) 
  {
   __cv_msg(
   "  Now writing statement and/or event trace output to file \"%s\".\n",
   __tr_fnam);
  }
 return;

done:
 __my_free(chp, slen + 1);
}

/*
 * execute a $scope change
 * this can be used for scope changes into local task from instance
 */
static void exec_expr_schg(struct expr_t *xp)
{
 struct itree_t *itp;
 struct task_t *tskp; 
 struct sy_t *syp;

 /* will not get here if no argument */

 /* need to handle scope change into local task - inst. does not change */
 /* scope changes of local [lb].[lb].[lb] simple task target by here */
 if (xp->optyp == ID)
  { 
   syp = xp->lu.sy;
   /* DBG remove */
   if (syp->sytyp != SYM_TSK && syp->sytyp != SYM_F && syp->sytyp != SYM_LB)
    __arg_terr(__FILE__, __LINE__);
   /* --- */
   __scope_tskp = xp->lu.sy->el.etskp;
   if (__iact_state) __set_scopchg_listline();
   return;
  } 

 /* DBG remove --- */
 if (xp->optyp != GLBREF) __arg_terr(__FILE__, __LINE__);
 /* --- */ 

 /* this converts from gref to itree location */
 __xmrpush_refgrp_to_targ(xp->ru.grp);
 itp = __inst_ptr;
 __pop_itstk();
 if (xp->lu.sy->sytyp != SYM_I && xp->lu.sy->sytyp != SYM_M)
  tskp = xp->lu.sy->el.etskp;
 else tskp = NULL;
 __scope_ptr = itp; 
 __scope_tskp = tskp;
 /* if called from interactive must update list line to scope */
 if (__iact_state)
  {
   /* in iact, need top of inst. stack to be same as scope ptr */
   __pop_itstk();
   __push_itstk(__scope_ptr);
   __set_scopchg_listline();

   if (__tfrec_hdr != NULL) __call_misctfs_scope();
   if (__have_vpi_actions) __vpi_iactscopechg_trycall();
  }
}

/*
 * emit various systask time end message - task passed >= 2 arg 
 */
extern void __emit_stsk_endmsg(void)
{
 /* notice must know current end time */
 __my_ftime(&__end_time, &__end_mstime);
 __prt_end_msg();
}
