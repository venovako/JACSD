#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  char name[MPI_MAX_PROCESSOR_NAME] = { '\0' };
  int size, rank, resultlen = 0;

  if (MPI_SUCCESS != MPI_Init(&argc, &argv))
    return EXIT_FAILURE;
  if (MPI_SUCCESS != MPI_Comm_size(MPI_COMM_WORLD, &size))
    return EXIT_FAILURE;
  if (MPI_SUCCESS != MPI_Comm_rank(MPI_COMM_WORLD, &rank))
    return EXIT_FAILURE;
  if (MPI_SUCCESS != MPI_Get_processor_name(name, &resultlen))
    return EXIT_FAILURE;
  if (MPI_MAX_PROCESSOR_NAME > resultlen)
    name[resultlen] = '\0';
  else
    return EXIT_FAILURE;
  if (!rank) {
    (void)fprintf(stdout, "%s: size = %5d, resultlen < %4d\n", *argv, size, MPI_MAX_PROCESSOR_NAME);
    (void)fflush(stdout);
  }
  if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD))
    return EXIT_FAILURE;
  (void)fprintf(stdout, "%5d %s\n", rank, name);
  if (MPI_SUCCESS != MPI_Finalize())
    return EXIT_FAILURE;
  return EXIT_SUCCESS;
}
