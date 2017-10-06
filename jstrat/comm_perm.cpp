// clang++ -DDistance=KNL_QHA2A -O3 -march=native -integrated-as -DNDEBUG -DN=4 -DStrat=ME0004 comm_perm.cpp
// clang++ -DDistance=KNL_SNC2  -O3 -march=native -integrated-as -DNDEBUG -DN=8 -DStrat=ME0008 comm_perm.cpp
// clang++ -DDistance=P100_4_1  -O3 -march=native -integrated-as -DNDEBUG -DN=8 -DStrat=ME0008 comm_perm.cpp

#include <algorithm>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <utility>

#ifndef N
#error N not defined
#endif // !N

#ifndef Strat
#error Strat not defined
#endif // !Strat

#if (N < 2)
#error N too small
#endif // ?N

#if (N % 2)
#error N not even
#endif // ?N

// define S to N for a quasi-cyclic strategy
// e.g., for the modified modulus; else, N-1
#ifndef S
#define S ((N) - 1)
#endif // !S

#ifndef S_
#define S_ ((S) - 1)
#else // S_
#error S_ already defined
#endif // !S_

#ifndef P
#define P ((N) >> 1)
#else // P
#error P already defined
#endif // !P

#ifndef P_
#define P_ ((P) - 1)
#else // P_
#error P_ already defined
#endif // !P_

#ifndef Int
#define Int int
#endif // !Int
typedef Int integer;
#include "jstrat_ME.h"

typedef unsigned Int natural;
typedef unsigned long Int Natural;

// good for enforcing a particular topology
#ifndef INF_DISTANCE
#define INF_DISTANCE 0U
#else // INF_DISTANCE
#error INF_DISTANCE already defined
#endif // !INF_DISTANCE

// the matrices of distances from a node i to a node j
// e.g., given by numa_distance(), in multiples of 10;
// if MPI gets involved, define by taking measurements
// of the rank-to-rank transfer time for large buffers
#ifdef Distance
#include "distances.h"
#else // !Distance
static natural Distance[P][P];
#endif // Distance

#ifndef INF_COST
#define INF_COST ~0UL
#else // INF_COST
#error INF_COST already defined
#endif // !INF_COST
static Natural min_cost = INF_COST;
#ifndef NO_COST
#define NO_COST 0UL
#else // NO_COST
#error NO_COST already defined
#endif // !NO_COST

typedef std::pair<integer,integer> pivot;
typedef std::pair<integer,integer> pcomm;

// the current permutation
static pivot curr[S][P];
// the best (cheapest) permutation so far
static pivot best[S][P];

// the current communication pattern
static pcomm comm_curr[S][P];
// the best (cheapest) communication pattern so far
static pcomm comm_best[S][P];

#ifndef Distance
// default distances (symmetric, banded)
static void init_distance()
{
  for (integer i = 0; i < P; ++i)
    for (integer j = 0; j < P; ++j)
      Distance[i][j] = (natural)(abs(i - j) + 1);
}
#endif // !Distance

static void init_curr()
{
  (void)memcpy((void*)&(curr[0][0]), (const void*)&(Strat[0][0][0]), sizeof(Strat));
}

static void sort_curr()
{
  for (integer i = 0; i < S; ++i)
    std::sort(&(curr[i][0]), &(curr[i][P]));
}

static bool new_best(const Natural new_cost)
{
  if (new_cost < min_cost) {
    min_cost = new_cost;
    (void)memcpy((void*)&(best[0][0]), (const void*)&(curr[0][0]), sizeof(best));
    (void)memcpy((void*)&(comm_best[0][0]), (const void*)&(comm_curr[0][0]), sizeof(comm_best));
    return true;
  }
  return false;
}

#ifndef NDEBUG
static volatile bool CtrlC = false;
static void int_handler(int)
{
  CtrlC = true;
}
#endif // !NDEBUG

static bool find_best(const integer s, const Natural cost)
{
  if (s == S) {
    if (cost > min_cost)
      return true;

    Natural step_cost = NO_COST;
    for (integer j = 0; j < P; ++j) {
#ifndef NDEBUG
      comm_curr[S_][j].second = comm_curr[S_][j].first = 0;
#endif // !NDEBUG
      for (integer k = 0; k < P; ++k) {
        if (curr[S_][j].first == curr[0][k].first) {
          comm_curr[S_][j].first = -(k + 1);
          if (Distance[j][k])
            step_cost += Distance[j][k];
          else {
            step_cost = INF_COST;
            goto check_final;
          }
        }
        else if (curr[S_][j].first == curr[0][k].second) {
          comm_curr[S_][j].first = k + 1;
          if (Distance[j][k])
            step_cost += Distance[j][k];
          else {
            step_cost = INF_COST;
            goto check_final;
          }
        }

        if (curr[S_][j].second == curr[0][k].first) {
          comm_curr[S_][j].second = -(k + 1);
          if (Distance[j][k])
            step_cost += Distance[j][k];
          else {
            step_cost = INF_COST;
            goto check_final;
          }
        }
        else if (curr[S_][j].second == curr[0][k].second) {
          comm_curr[S_][j].second = k + 1;
          if (Distance[j][k])
            step_cost += Distance[j][k];
          else {
            step_cost = INF_COST;
            goto check_final;
          }
        }
      }
    }

  check_final:
    if (step_cost != INF_COST) {
      if (new_best(cost + step_cost)) {
        (void)fprintf(stdout, "new min-cost found: %lu\n", min_cost);
        (void)fflush(stdout);
      }
    }
#ifdef NDEBUG
    return true;
#else // !NDEBUG
    return (CtrlC ? false : true);
#endif // NDEBUG
  }
  else if (s > 0) {
    if (cost > min_cost)
      return true;

    const integer s_ = s - 1;
    do {
      Natural step_cost = NO_COST;
      for (integer j = 0; j < P; ++j) {
#ifndef NDEBUG
        comm_curr[s_][j].second = comm_curr[s_][j].first = 0;
#endif // !NDEBUG
        for (integer k = 0; k < P; ++k) {
          if (curr[s_][j].first == curr[s][k].first) {
            comm_curr[s_][j].first = -(k + 1);
            if (Distance[j][k])
              step_cost += Distance[j][k];
            else {
              step_cost = INF_COST;
              goto check_cost;
            }
          }
          else if (curr[s_][j].first == curr[s][k].second) {
            comm_curr[s_][j].first = k + 1;
            if (Distance[j][k])
              step_cost += Distance[j][k];
            else {
              step_cost = INF_COST;
              goto check_cost;
            }
          }

          if (curr[s_][j].second == curr[s][k].first) {
            comm_curr[s_][j].second = -(k + 1);
            if (Distance[j][k])
              step_cost += Distance[j][k];
            else {
              step_cost = INF_COST;
              goto check_cost;
            }
          }
          else if (curr[s_][j].second == curr[s][k].second) {
            comm_curr[s_][j].second = k + 1;
            if (Distance[j][k])
              step_cost += Distance[j][k];
            else {
              step_cost = INF_COST;
              goto check_cost;
            }
          }
        }
      }

    check_cost:
      if (step_cost == INF_COST)
        continue;
      if (!find_best((s + 1), (cost + step_cost)))
        return false;
    } while (next_permutation(&(curr[s][0]), &(curr[s][P])));
    return true;
  }
  else if (!s) {
    if (cost > min_cost)
      return false;

    do {
      if (!find_best((s + 1), cost))
        return false;
    } while (next_permutation(&(curr[s][0]), &(curr[s][P])));
    return true;
  }

  return false;
}

int main(int argc, char* argv[])
{
#ifndef Distance
  init_distance();
#endif // !Distance
  init_curr();
  sort_curr();
#ifndef NDEBUG
  (void)signal(SIGINT, int_handler);
#endif // !NDEBUG
  const bool ret = find_best(0, NO_COST);
  if (min_cost == INF_COST)
    goto the_end;

  (void)fprintf(stdout, "\nstatic const integer me%04u[%4u][%4u][2] =\n{\n", (natural)N, (natural)S, (natural)P);
  for (integer i = 0; i < S; ++i) {
    (void)fprintf(stdout, "  {");
    for (integer j = 0; j < P; ++j) {
#if (N <= 10)
      (void)fprintf(stdout, "{%1u,%1u}", (natural)(best[i][j].first), (natural)(best[i][j].second));
#elif (N <= 100)
      (void)fprintf(stdout, "{%2u,%2u}", (natural)(best[i][j].first), (natural)(best[i][j].second));
#elif (N <= 1000)
      (void)fprintf(stdout, "{%3u,%3u}", (natural)(best[i][j].first), (natural)(best[i][j].second));
#elif (N <= 10000)
      (void)fprintf(stdout, "{%4u,%4u}", (natural)(best[i][j].first), (natural)(best[i][j].second));
#else // N > 10000
      (void)fprintf(stdout, "{%u,%u}", (natural)(best[i][j].first), (natural)(best[i][j].second));
#endif // ?N
      if (j < P_)
        (void)fprintf(stdout, ",");
    }
    (void)fprintf(stdout, ((i < S_) ? "},\n" : "}\n"));
  }
  (void)fprintf(stdout, "};\n");
  // communication
  (void)fprintf(stdout, "static const integer comm%04u[%4u][%4u][2] =\n{\n", (natural)N, (natural)S, (natural)P);
  for (integer i = 0; i < S; ++i) {
    (void)fprintf(stdout, "  {");
    for (integer j = 0; j < P; ++j) {
#if (N <= 10)
      (void)fprintf(stdout, "{%2d,%2d}", comm_best[i][j].first, comm_best[i][j].second);
#elif (N <= 100)
      (void)fprintf(stdout, "{%3d,%3d}", comm_best[i][j].first, comm_best[i][j].second);
#elif (N <= 1000)
      (void)fprintf(stdout, "{%4d,%4d}", comm_best[i][j].first, comm_best[i][j].second);
#elif (N <= 10000)
      (void)fprintf(stdout, "{%5d,%5d}", comm_best[i][j].first, comm_best[i][j].second);
#else // N > 10000
      (void)fprintf(stdout, "{%d,%d}", comm_best[i][j].first, comm_best[i][j].second);
#endif // ?N
      if (j < P_)
        (void)fprintf(stdout, ",");
    }
    (void)fprintf(stdout, ((i < S_) ? "},\n" : "}\n"));
  }
  (void)fprintf(stdout, "};\n");

 the_end:
  return (ret ? EXIT_SUCCESS : EXIT_FAILURE);
}
