CXXFLAGS += -std=c++11 -Wall -g

all:
	# build runtime .a file
	$(CXX) $(CXXFLAGS) -c -o runtime.o systest_dpor.cpp -pthread
	$(CXX) $(CXXFLAGS) -c -o exit_tracker.o exit_tracker.cpp -pthread
	$(CXX) $(CXXFLAGS) -c -o socket_msg.o socket_msg.cpp
	ar rcs libsystest_runtime.a *.o
