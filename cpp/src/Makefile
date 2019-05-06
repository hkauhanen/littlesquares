all: littlesquares littlesquares-graph

littlesquares: DataFrame.o GraphWorld.o Language.o LatticeWorld.o main.o RNG.o SomeMath.o StringFrame.o Utils.o World.o
	g++ -static DataFrame.o GraphWorld.o Language.o LatticeWorld.o main.o RNG.o SomeMath.o StringFrame.o Utils.o World.o -o littlesquares
	cp littlesquares ..
	rm littlesquares

littlesquares-graph: DataFrame.o GraphWorld.o Language.o LatticeWorld.o main-graph.o RNG.o SomeMath.o StringFrame.o Utils.o World.o
	g++ -static DataFrame.o GraphWorld.o Language.o LatticeWorld.o main-graph.o RNG.o SomeMath.o StringFrame.o Utils.o World.o -o littlesquares-graph
	cp littlesquares-graph ..
	rm littlesquares-graph

DataFrame.o: DataFrame.cpp
	g++ -c -static DataFrame.cpp -o DataFrame.o

GraphWorld.o: GraphWorld.cpp
	g++ -c -static GraphWorld.cpp -o GraphWorld.o

Language.o: Language.cpp
	g++ -c -static Language.cpp -o Language.o

LatticeWorld.o: LatticeWorld.cpp
	g++ -c -static LatticeWorld.cpp -o LatticeWorld.o

main.o: main.cpp
	g++ -c -static main.cpp -o main.o

main-graph.o: main-graph.cpp
	g++ -c -static main-graph.cpp -o main-graph.o

RNG.o: RNG.cpp
	g++ -c -static RNG.cpp -o RNG.o

SomeMath.o: SomeMath.cpp
	g++ -c -static SomeMath.cpp -o SomeMath.o

StringFrame.o: StringFrame.cpp
	g++ -c -static StringFrame.cpp -o StringFrame.o

Utils.o: Utils.cpp
	g++ -c -static Utils.cpp -o Utils.o

World.o: World.cpp
	g++ -c -static World.cpp -o World.o

clean:
	rm *.o
