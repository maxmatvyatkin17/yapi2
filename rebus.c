#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MAX_PUZZLE_LEN 512
#define MIN_OPERANDS 2
#define MAX_OPERANDS 7
#define MAX_WORD_LEN 18
#define MAX_LETTERS 10

typedef __int128 i128;

typedef struct {
    char text[MAX_WORD_LEN + 1];
    int idx[MAX_WORD_LEN];
    int len;
} Word;

static Word g_words[MAX_OPERANDS + 1];
static char g_ops[MAX_OPERANDS];
static int g_noperands;

static int g_word_id[MAX_OPERANDS + 1];
static int g_unique[MAX_OPERANDS + 1];
static int g_nunique;
static int g_is_leading[MAX_LETTERS];

static const char *g_puzzle;
static char g_letters[26 + 1];
static int g_nletters;
static int g_index_of[26];
static int g_digit[MAX_LETTERS];
static int g_used[10];
static char g_candidate[MAX_PUZZLE_LEN + 1];

static int is_op(char c) { return c == '+' || c == '-' || c == '*' || c == '/'; }

static const char *skip_spaces(const char *p)
{
    while (*p == ' ' || *p == '\t')
        p++;
    return p;
}

static const char *read_word(const char *p, Word *w)
{
    w->len = 0;
    while (isupper((unsigned char)*p)) {
        if (w->len == MAX_WORD_LEN)
            return NULL;
        w->text[w->len++] = *p++;
    }
    w->text[w->len] = '\0';
    return w->len > 0 ? p : NULL;
}

static int parse_puzzle(const char *s)
{
    const char *p = skip_spaces(s);
    if (strlen(s) > MAX_PUZZLE_LEN)
        return 0;

    g_noperands = 0;
    for (;;) {
        if (g_noperands == MAX_OPERANDS)
            return 0;
        p = read_word(p, &g_words[g_noperands]);
        if (!p)
            return 0;
        g_noperands++;
        p = skip_spaces(p);
        if (*p == '=')
            break;
        if (!is_op(*p))
            return 0;
        g_ops[g_noperands - 1] = *p;
        p = skip_spaces(p + 1);
    }
    if (g_noperands < MIN_OPERANDS)
        return 0;
    p = read_word(skip_spaces(p + 1), &g_words[g_noperands]);
    if (!p || *skip_spaces(p) != '\0')
        return 0;

    g_nletters = 0;
    g_letters[0] = '\0';
    for (const char *c = s; *c; c++) {
        if (isupper((unsigned char)*c) && !strchr(g_letters, *c)) {
            g_index_of[*c - 'A'] = g_nletters;
            g_letters[g_nletters++] = *c;
            g_letters[g_nletters] = '\0';
        }
    }
    for (int i = 0; i <= g_noperands; i++)
        for (int j = 0; j < g_words[i].len; j++)
            g_words[i].idx[j] = g_index_of[g_words[i].text[j] - 'A'];

    g_nunique = 0;
    memset(g_is_leading, 0, sizeof g_is_leading);
    for (int i = 0; i <= g_noperands; i++) {
        int u = 0;
        while (u < g_nunique && strcmp(g_words[g_unique[u]].text, g_words[i].text) != 0)
            u++;
        if (u == g_nunique)
            g_unique[g_nunique++] = i;
        g_word_id[i] = u;
        if (g_words[i].len > 1 && g_words[i].idx[0] < MAX_LETTERS)
            g_is_leading[g_words[i].idx[0]] = 1;
    }
    return 1;
}

static long long word_value(const Word *w)
{
    long long v = 0;
    for (int i = 0; i < w->len; i++)
        v = v * 10 + g_digit[w->idx[i]];
    return v;
}

static int check_assignment(void)
{
    long long value[MAX_OPERANDS + 1];
    for (int u = 0; u < g_nunique; u++)
        value[u] = word_value(&g_words[g_unique[u]]);

    i128 total = 0, term = value[g_word_id[0]];
    int sign = 1;
    for (int i = 1; i < g_noperands; i++) {
        long long v = value[g_word_id[i]];
        char op = g_ops[i - 1];
        if (op == '*') {
            if (__builtin_mul_overflow(term, (i128)v, &term))
                return 0;
        } else if (op == '/') {
            if (v == 0 || term % v != 0)
                return 0;
            term /= v;
        } else {
            if (__builtin_add_overflow(total, sign * term, &total))
                return 0;
            sign = (op == '+') ? 1 : -1;
            term = v;
        }
    }
    if (__builtin_add_overflow(total, sign * term, &total))
        return 0;
    return total == value[g_word_id[g_noperands]];
}

static void build_candidate(void)
{
    int i;
    for (i = 0; g_puzzle[i]; i++) {
        char c = g_puzzle[i];
        if (isupper((unsigned char)c)) {
            g_candidate[i] = (char)('0' + g_digit[g_index_of[c - 'A']]);
        } else {
            g_candidate[i] = c;
        }
    }
    g_candidate[i] = '\0';
}

static int search(int pos)
{
    if (pos == g_nletters)
        return check_assignment();
    for (int d = g_is_leading[pos] ? 1 : 0; d <= 9; d++) {
        if (g_used[d])
            continue;
        g_used[d] = 1;
        g_digit[pos] = d;
        if (search(pos + 1))
            return 1;
        g_used[d] = 0;
    }
    return 0;
}

int solve_rebus(const char *puzzle, char *out, size_t out_size)
{
    if (!puzzle || !out || !parse_puzzle(puzzle))
        return -1;
    if (strlen(puzzle) + 1 > out_size)
        return -1;

    if (g_nletters > MAX_LETTERS)
        return 0;

    g_puzzle = puzzle;
    memset(g_used, 0, sizeof g_used);
    if (!search(0))
        return 0;

    build_candidate();
    strcpy(out, g_candidate);
    return 1;
}

static int verify(const char *puzzle, const char *sol)
{
    int letter_digit[26], digit_letter[10];
    memset(letter_digit, -1, sizeof letter_digit);
    memset(digit_letter, -1, sizeof digit_letter);
    if (strlen(puzzle) != strlen(sol))
        return 0;
    for (size_t i = 0; puzzle[i]; i++) {
        char p = puzzle[i], s = sol[i];
        if (p >= 'A' && p <= 'Z') {
            if (s < '0' || s > '9')
                return 0;
            int l = p - 'A', d = s - '0';
            if ((letter_digit[l] != -1 && letter_digit[l] != d) ||
                (digit_letter[d] != -1 && digit_letter[d] != l))
                return 0;
            letter_digit[l] = d;
            digit_letter[d] = l;
        } else if (p != s) {
            return 0;
        }
    }

    __int128 total = 0, term = 0, v;
    int sign = 1;
    char op = '+';
    const char *q = sol;
    for (;;) {
        while (*q == ' ')
            q++;
        if (*q < '0' || *q > '9' || (q[0] == '0' && q[1] >= '0' && q[1] <= '9'))
            return 0;
        for (v = 0; *q >= '0' && *q <= '9'; q++)
            v = v * 10 + (*q - '0');
        if (op == '=')
            return *q == '\0' && total == v;
        if (op == '*')
            term *= v;
        else if (op == '/') {
            if (v == 0 || term % v != 0)
                return 0;
            term /= v;
        } else {
            total += sign * term;
            sign = (op == '-') ? -1 : 1;
            term = v;
        }
        while (*q == ' ')
            q++;
        op = *q++;
        if (op == '=')
            total += sign * term;
        else if (op != '+' && op != '-' && op != '*' && op != '/')
            return 0;
    }
}

static const char *const BASIC[] = {
    "SEND + MORE = MONEY",
    "BE + BE = MOO",
    "BIG + CAT = LION",
    "TO + GO = OUT",
    "CROSS + ROADS = DANGER",
    "DONALD + GERALD = ROBERT",
    "FORTY + TEN + TEN = SIXTY",
    "ELEVEN + NINE + FIVE + FIVE = THIRTY",
    "SATURN + URANUS + NEPTUNE + PLUTO = PLANETS",
    "COUNT - COIN = SNUB",
    "XAHCAJ - ZJZUJ = ZTCDDO",
    "TILXJM - XKQQZ = KKZTXC",
    "UBK * LPV = YQABB",
    "WXF * OM = RUYGM",
    "DU * UG * OP = QKQYUV",
    "MVANBV / JIZ = NLV",
    "KRSYYVQ / ZSQ = NFKQ",
    "NWL * GV + SGWE = YMNEG",
    "EOKKY - IXOKD / RE = EIVXE",
    "QGU + ZGU + NRVU + GRUU + QQP + GGYY + PRRQ = PNVPR",
    "YHYE - AFOF - KAGRF + EEAO + OOEFE = AOOEO",
    NULL
};

static const char *const HARD[] = {
    "WMGBOC - DDDK = WUCGFW",
    "GOPKLW - RRRVB = GCOWOB",
    "YIBVW - LTLS = YKBZV",
    "AHFVKT - KTJI = KABQBI",
    "EWZX * HHO = ECDDMI",
    "ICFZ * IHU = DOUHTOX",
    "IDIL * QFT = MLBQVRV",
    "RRBZ * IPN = KTIJZLN",
    "OSSEA * DDP = DGHODIPC",
    "UABCZ * AUI = XBMCCWVZ",
    "YNUVL * UPF = DMEFMPEL",
    "HEYKZ * NKZ = JNTWKAZZ",
    "TO * IU * EN = SOMBH",
    "FZ * YJ * BG = GNETM",
    "CC * NM * YH = KZVUB",
    "DQ * MN * QO = ZPPAWC",
    "OPHHLWB / XXK = PYFP",
    "KEZHXD / ERT = ZIPD",
    "TOYEQEK / BBX = KYAP",
    "VMJDJME / TEQ = YHBE",
    "XFBND / ND * LK = XXAZO",
    "SIDNU / DO * PS = SSABE",
    "KBQCS / GX * XD = VGBAV",
    "KFDKR / DW * RH = HCYBA",
    "DVL * YB + JHBY = VIMLA",
    "CPO * DB + GLYN = BDIPB",
    "WNR * VS + GQPP = VLHQN",
    "EUO * CW + ERBJ = ICXIX",
    "FJBE / UH + NUVB = NXSE",
    "GGJIP / VJ + XCVL = JBTC",
    "ZDPAS / RS + BDRW = BONR",
    "CTFBI / VK + CSTG = IIFY",
    "BBBNF - JNKDE / QW = BBDYF",
    "GGBRX - XZCDG / GE = GRMZI",
    "FCEAN - FXPXE / BY = FZZPE",
    "EJJRG - JWERZ / KQ = EGKPS",
    "QHII + PUO + TDUT + QIL + LDH + OJUH + QOO = DDQJT",
    "AACU + HXLC + CHZC + XALN + GGXX + NXNG + XGHG = GHAGI",
    "VVID + PPV + GGZZ + DPG + PGDL + LDYX + YGGI = FYGPL",
    "ZIS + VVW + ZLSK + WSFL + VIL + WHK + ZIS = LSBWS",
    "JJKKD + LMLN - KDJO + JAKMH + HNNG = AONNA",
    "FXMB - UHUH - FVPB - XTVH + HTMPM = OBBHH",
    "IJIY + YKLLK - MYZRY + YYKMK - HGRIJ = IGRZG",
    "OHGM + RYYO - ALHAY + YLFVM - RGVO = OOLMF",
    NULL
};

static double run(const char *puzzle)
{
    char out[1024];
    int r, runs = 0;
    clock_t start = clock();
    double elapsed;
    do {
        r = solve_rebus(puzzle, out, sizeof out);
        runs++;
        elapsed = (double)(clock() - start) / CLOCKS_PER_SEC;
    } while (elapsed < 0.1);
    double ms = elapsed / runs * 1000.0;

    if (r == 1)
        printf("%-50s  %-50s  %s  %10.4f ms\n", puzzle, out,
               verify(puzzle, out) ? "OK  " : "FAIL", ms);
    else
        printf("%-50s  %s\n", puzzle, r == 0 ? "no solution" : "bad input");
    return ms;
}

static double run_set(const char *title, const char *const *set)
{
    double total = 0;
    printf("\n=== %s ===\n", title);
    for (int i = 0; set[i]; i++)
        total += run(set[i]);
    printf("Total: %.4f ms\n", total);
    return total;
}

int main(int argc, char **argv)
{
    if (argc > 1) {
        for (int i = 1; i < argc; i++)
            run(argv[i]);
        return 0;
    }
    double basic = run_set("BASIC (21 puzzles)", BASIC);
    double hard = run_set("HARD (44 puzzles, 10 letters each)", HARD);
    printf("\nBASIC total: %.4f ms\nHARD  total: %.4f ms\n", basic, hard);
    return 0;
}
