package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/exp/constraints"
)

func Abs[T constraints.Integer](x T) T {
	if x < 0 {
		return -x
	}
	return x
}

func processLine(s string) (int, int) {
	parts := strings.Split(s, " ")
	realParts := []string{}

	for _, p := range parts {
		if p == "" {
			continue
		}
		realParts = append(realParts, p)
	}

	num1, err := strconv.Atoi(realParts[0])
	if err != nil {
		panic(err)
	}

	num2, err := strconv.Atoi(realParts[1])
	if err != nil {
		panic(err)
	}

	return num1, num2
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	a1 := make([]int, 1000)
	a2 := make([]int, 1000)
	i := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		a1[i], a2[i] = processLine(line)
		i++
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	// Part 1
	sort.Ints(a1)
	sort.Ints(a2)

	count := 0

	for i := 0; i < len(a1); i++ {
		count += Abs(a1[i] - a2[i])
	}
	fmt.Println("Part 1:", count)

	// Part 2
	count = 0
	myMap := make(map[int]int)

	for _, v := range a2 {
		myMap[v] += 1
	}

	for _, v := range a1 {
		count += v * myMap[v]
	}

	fmt.Println("Part 2:", count)
}
