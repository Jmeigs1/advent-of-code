SRC=$(shell find . -name '*.cs')

run: build/csharp
	@./build/csharp
build/csharp: $(SRC)
	@dotnet publish -o build
