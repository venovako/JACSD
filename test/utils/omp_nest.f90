program omp_nest
  use omp_lib
  implicit none
  print *, omp_get_max_active_levels()
end program omp_nest
