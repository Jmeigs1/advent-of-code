package main

import (
	"bufio"
	"fmt"
	"os"
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

func parseToInts(s string) []int {
	parts := strings.Fields(s)
	ret := []int{}

	for _, p := range parts {
		num, err := strconv.Atoi(p)
		if err != nil {
			panic(err)
		}

		ret = append(ret, num)

	}

	return ret
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	inputData := [][]int{}
	i := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		lineArray := parseToInts(line)
		inputData = append(inputData, lineArray)
		i++
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	part1(inputData)
	part2(inputData)
}

func part1(inputData [][]int) {
	count := 0

	for _, a := range inputData {
		safe := true
		base := a[1] - a[0]

		if base == 0 {
			// Not safe
			continue
		}

		for i := 1; i < len(a); i++ {
			diff := a[i] - a[i-1]
			if diff*base <= 0 {
				safe = false
				break
			}
			if Abs(diff) > 3 {
				safe = false
				break
			}
		}
		if safe {
			count++
		}
	}
	fmt.Println("Part1:", count)
}

func part2(inputData [][]int) {
	count := 0
	count2 := 0
	count3 := 0

	for j, a := range inputData {
		bad, removed := badRow(a, j, false)

		if !bad {
			count++
		}
		if !bad && !removed {
			count2++
		}
		if !bad && removed {
			count3++
		}
	}
	fmt.Println("Part2:", count)
	fmt.Println("Part2 no removed:", count2)
	fmt.Println("Part2 removed:", count3)
}

func debugRow(a []int, j int) bool {
	bad, removed := badRow(a, j, false)
	fmt.Println(a, j)
	fmt.Println("res", bad)
	fmt.Println("removed", removed)
	fmt.Println("")
	return bad
}

func remove(slice []int, s int) []int {
	newSlice := []int{}
	for i := 0; i < len(slice); i++ {
		if i == s {
			continue
		}
		newSlice = append(newSlice, slice[i])
	}
	return newSlice
}

func badRow(a []int, j int, removed bool) (bool, bool) {
	base := findBase(a)

	if base == 0 {
		panic(fmt.Sprintf("Base is 0 at row :%d", j))
	}
	for i := 1; i < len(a); i++ {
		badsite := isBadSite(a[i], a[i-1], base)

		if badsite {
			if removed {
				return true, removed
			}

			for k := 0; k < len(a); k++ {
				a1 := remove(a, k)
				isBad1, _ := badRow(a1, j, true)
				if !isBad1 {
					return false, true
				}
			}

			return true, false
		}
	}
	return false, removed
}

func isBadSite(first int, second int, base int) bool {
	diff := first - second
	if diff*base <= 0 {
		return true
	} else if Abs(diff) > 3 {
		return true
	}
	return false
}

func findBase(a []int) int {
	for i := 1; i < len(a); i++ {
		base := a[i] - a[i-1]
		if base != 0 {
			return base
		}
	}
	return 0
}
