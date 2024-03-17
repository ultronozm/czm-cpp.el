((cmake-build-cmake-profiles
  (release "-DCMAKE_BUILD_TYPE=Release")
  (debug "-DCMAKE_BUILD_TYPE=Debug"))

 (cmake-build-run-configs
  (all (:build "test") (:run nil "test" ""))))
