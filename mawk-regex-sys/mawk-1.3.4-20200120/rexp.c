/********************************************
rexp.c
copyright 2008-2016,2017, Thomas E. Dickey
copyright 1991-1993,1996, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*
 * $MawkId: rexp.c,v 1.21 2017/10/17 01:19:15 tom Exp $
 */

/*  op precedence  parser for regular expressions  */

#include "rexp.h"
#include "regexp.h"

/*  DATA   */
int REerrno;
const char *const REerrlist[] =
{(char *) 0,
 /* 1  */ "missing '('",
 /* 2  */ "missing ')'",
 /* 3  */ "bad class -- [], [^] or [",
 /* 4  */ "missing operand",
 /* 5  */ "resource exhaustion -- regular expression too large",
 /* 6  */ "syntax error ^* or ^+"
};
/* E5 is very unlikely to occur */

/* This table drives the operator precedence parser */
/* *INDENT-OFF* */
static  short  table[8][8] = {
/*        0   |   CAT  *   +   ?   (   )   */
/* 0 */  {0,  L,  L,   L,  L,  L,  L,  E1},
/* | */  {G,  G,  L,   L,  L,  L,  L,  G},
/* CAT*/ {G,  G,  G,   L,  L,  L,  L,  G},
/* * */  {G,  G,  G,   G,  G,  G,  E7, G},
/* + */  {G,  G,  G,   G,  G,  G,  E7, G},
/* ? */  {G,  G,  G,   G,  G,  G,  E7, G},
/* ( */  {E2, L,  L,   L,  L,  L,  L,  EQ},
/* ) */  {G , G,  G,   G,  G,  G,  E7, G}};
/* *INDENT-ON* */

#define	 STACKSZ   64

static jmp_buf err_buf;		/*  used to trap on error */

#if OPT_TRACE > 0
static const char *
token_name(int token)
{
    const char *result;
#define CASE(name) case name: result = #name; break
    switch (token) {
	CASE(T_NONE);
	CASE(T_OR);
	CASE(T_CAT);
	CASE(T_STAR);
	CASE(T_PLUS);
	CASE(T_Q);
	CASE(T_LP);
	CASE(T_RP);
	CASE(T_START);
	CASE(T_END);
	CASE(T_ANY);
	CASE(T_CLASS);
	CASE(T_SLASH);
	CASE(T_CHAR);
	CASE(T_STR);
	CASE(T_U);
    default:
	result = "?";
	break;
    }
#undef CASE
    return result;
}
#endif

void
RE_error_trap(int x)
{
    REerrno = x;
    longjmp(err_buf, 1);
}

PTR
REcompile(char *re, size_t len)
{
    MACHINE m_stack[STACKSZ];
    struct op {
	int token;
	int prec;
    } op_stack[STACKSZ];
    register MACHINE *m_ptr;
    register struct op *op_ptr;
    register int t;

    /* do this first because it also checks if we have a
       run time stack */
    RE_lex_init(re, len);

    if (len == 0) {
	STATE *p = (STATE *) RE_malloc(sizeof(STATE));
	p->s_type = M_ACCEPT;
	return (PTR) p;
    }

    if (setjmp(err_buf))
	return (PTR) 0;
    /* we used to try to recover memory left on machine stack ;
       but now m_ptr is in a register so it won't be right unless
       we force it out of a register which isn't worth the trouble */

    /* initialize the stacks  */
    m_ptr = m_stack - 1;
    op_ptr = op_stack;
    op_ptr->token = 0;

    t = RE_lex(m_stack);

    while (1) {
	TRACE(("RE_lex token %s\n", token_name(t)));
	switch (t) {
	case T_STR:
	case T_ANY:
	case T_U:
	case T_START:
	case T_END:
	case T_CLASS:
	    m_ptr++;
	    break;

	case 0:		/*  end of reg expr   */
	    if (op_ptr->token == 0) {
		/*  done   */
		if (m_ptr == m_stack) {
		    return (PTR) m_ptr->start;
		} else {
		    /* machines still on the stack  */
		    RE_panic("values still on machine stack");
		}
	    }
	    /* FALLTHRU */

	    /*  otherwise, default is operator case  */

	default:

	    if ((op_ptr->prec = table[op_ptr->token][t]) == G) {
		do {		/* op_pop   */

		    if (op_ptr->token <= T_CAT) {	/*binary op */
			if (m_ptr == m_stack
			    && op_ptr->token == T_CAT) {
			    TRACE(("...ignoring empty T_CAT\n"));
			    op_ptr--;
			    continue;
			}
			m_ptr--;
		    }
		    /* if not enough values on machine stack
		       then we have a missing operand */
		    if (m_ptr < m_stack)
			RE_error_trap(-E4);

		    switch (op_ptr->token) {
		    case T_CAT:
			RE_cat(m_ptr, m_ptr + 1);
			break;

		    case T_OR:
			RE_or(m_ptr, m_ptr + 1);
			break;

		    case T_STAR:
			RE_close(m_ptr);
			break;

		    case T_PLUS:
			RE_poscl(m_ptr);
			break;

		    case T_Q:
			RE_01(m_ptr);
			break;

		    default:
			/*nothing on ( or ) */
			break;
		    }

		    op_ptr--;
		}
		while (op_ptr->prec != L);

		continue;	/* back thru switch at top */
	    }

	    if (op_ptr->prec < 0) {
		if (op_ptr->prec == E7)
		    RE_panic("parser returns E7");
		else
		    RE_error_trap(-op_ptr->prec);
	    }

	    if (++op_ptr == op_stack + STACKSZ) {
		/* stack overflow */
		RE_error_trap(-E5);
	    }

	    op_ptr->token = t;
	}			/* end of switch */

	if (m_ptr == m_stack + (STACKSZ - 1)) {
	    /*overflow */
	    RE_error_trap(-E5);
	}

	t = RE_lex(m_ptr + 1);
    }
}

void
REdestroy(PTR ptr)
{
    int done = 0;
    int n = 0;
    STATE *q = (STATE *) ptr;

    TRACE(("REdestroy %p\n", ptr));
    while (!done) {
	TRACE(("...%d type %d\n", n, q->s_type));
	switch (q->s_type) {
	case M_ACCEPT:
	    done = 1;
	    break;
	case M_STR:
	    RE_free(q->s_data.str);
	    break;
	default:
	    if (q->s_type < 0 || q->s_type > END_ON)
		done = -1;
	    break;
	}
	++q;
	++n;
    }
    RE_free(ptr);
}

/* getting here means a logic flaw or unforeseen case */
void
RE_panic(const char *s)
{
    fprintf(stderr, "REcompile() - panic:  %s\n", s);
//    mawk_exit(100); TODO: WE should actually panic or something
}

/* getting regexp error message */
const char *
REerror(void)
{
    return REerrlist[REerrno];
}
