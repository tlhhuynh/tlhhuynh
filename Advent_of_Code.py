#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec  5 16:40:23 2022

@author: lhuynh
"""

"""

Day 1

"""
day1 = open('day1.txt', 'r').read().splitlines()

i = 0
j = 0
k = 0
sum = {}

while i < len(day1):
    if len(day1[i]) != 0:
        k = k + int(day1[i])
    else:
        j = j + 1
        k = 0
    i = i + 1
    sum[j] = k
    
max(sum.values())

sum_2 = {l: v for l, v in sorted(sum.items(), key=lambda item: item[1])}

total = 0
for x in list(sum_2)[-3:]:
    total = total + sum_2[x]
print(total)

"""

Day 2

"""
day2 = open('day2.txt', 'r').read().splitlines()
x = 1
y = 2
z = 3
lose = 0
draw = 3
win = 6
#function to check wins
def game(player1, player2):
    score = 0
    if player2 == 'X':
        score = score + x
        if player1 == 'A':
            score = score + draw
        elif player1 == 'B':
            score = score + lose
        else:
            score = score + win
    elif player2 == 'Y':
        score = score + y
        if player1 == 'A':
            score = score + win
        elif player1 == 'B':
            score = score + draw
        else:
            score = score + lose
    else:
        score = score + z
        if player1 == 'A':
            score = score + lose
        elif player1 == 'B':
            score = score + win
        else:
            score = score + draw
    return score
    
i = 0
runningscore = 0
while i < len(day2):
    runningscore = runningscore + game(day2[i][0],day2[i][2])
    i = i + 1
print(runningscore)

lose = 0
draw = 3
win = 6
rock = 1
paper = 2
scissor = 3
#function to check wins
def gameNEW(player1, player2):
    score = 0
    if player2 == 'X':
        score = score + lose
        if player1 == 'A':
            score = score + scissor
        elif player1 == 'B':
            score = score + rock
        else:
            score = score + paper
    elif player2 == 'Y':
        score = score + draw
        if player1 == 'A':
            score = score + rock
        elif player1 == 'B':
            score = score + paper
        else:
            score = score + scissor
    else:
        score = score + win
        if player1 == 'A':
            score = score + paper
        elif player1 == 'B':
            score = score + scissor
        else:
            score = score + rock
    return score
    
i = 0
runningscoreNEW = 0
while i < len(day2):
    runningscoreNEW = runningscoreNEW + gameNEW(day2[i][0],day2[i][2])
    i = i + 1
print(runningscoreNEW)
    
"""

Day 3

""" 
day3 = open('day3.txt', 'r').read().splitlines()

matched = []
i = 0
j = 0
length = len(day3)

while i < length:
    half = int(len(day3[i])/2)
    while j <= half:
        if day3[i].find(day3[i][j],half) != -1:
            matched.append(day3[i][j])
            break
        j+=1
    i += 1
    j = 0
    
alpha1 = [count for count in range(1,53)]
import string
alpha2 = [letter for letter in string.ascii_letters]

priority = dict(zip(alpha2,alpha1))
    
i = 0
priorityScore = 0
while i < len(matched):
    priorityScore = priorityScore + priority[matched[i]]
    i = i + 1
print(priorityScore)

badge = []
i = 0
j = 0
length = len(day3)

while i < length:
    elf2 = i + 1
    elf3 = i + 2
    while j <= len(day3[i]):
        if day3[elf2].find(day3[i][j]) != -1 and day3[elf3].find(day3[i][j]) != -1 :
            badge.append(day3[i][j])
            break
        j+=1
    i += 3
    j = 0
    
i = 0
priorityScore = 0
while i < len(badge):
    priorityScore = priorityScore + priority[badge[i]]
    i = i + 1
print(priorityScore)
    
"""

Day 4

"""  
day4 = open('day4.txt', 'r').read().splitlines()
sample = ["2-4,6-8",
"2-3,4-5",
"5-7,7-9",
"2-8,3-7",
"6-6,4-6",
"2-6,4-8"]

import re
pattern = r'\W+'
i = 0
sList = []
while i < len(day4):
    sList.append(re.split(pattern,day4[i]))
    i+=1

list2 = []
i = 0
while i <len(sList):
    list2.append([int(x) for x in sList[i]])
    i +=1
    
i = 0
assign = 0
while i < len(list2):
    if list2[i][0] in range(list2[i][2],int(list2[i][3]+1)) and list2[i][1] in range(list2[i][2],int(list2[i][3]+1)):
        assign +=1
    elif list2[i][2] in range(list2[i][0],int(list2[i][1]+1)) and list2[i][3] in range(list2[i][0],int(list2[i][1]+1)):
        assign +=1
    i = i + 1
print(assign)

i = 0
assign2 = 0
while i < len(list2):
    if list2[i][0] in range(list2[i][2],int(list2[i][3]+1)) or list2[i][1] in range(list2[i][2],int(list2[i][3]+1)):
        assign2 +=1
    elif list2[i][2] in range(list2[i][0],int(list2[i][1]+1)) or list2[i][3] in range(list2[i][0],int(list2[i][1]+1)):
        assign2 +=1
    i = i + 1
print(assign2)

"""

Day 5

"""  
day5 = open('day5.txt', 'r').read().splitlines()
import pandas as pd

#manipulate crates
crates = pd.Series(day5[0:9])
expand = crates.str.split('',expand=True)
simple =  expand.iloc[:,[2,6,10,14,18,22,26,30,34]]
simple = simple.rename(columns={2:1,6:2,10:3,14:4,18:5,22:6,26:7,30:8,34:9})
simple1 = simple.iloc[0:8,:]
simple2 = simple1.sort_index(ascending=False)  

#create a list for each crate
for i in range(len(simple2.iloc[0])):

    # for 0-4, list_name = 'list_1'
    list_name = 'crate_' + str(i+1)
    # Try to append to that list
    try:
        globals()[list_name].append(simple2[simple2.columns[i]].values.tolist())
    # If if doesn't exist, create it on the run!
    except KeyError:
        globals()[list_name] = simple2[simple2.columns[i]].values.tolist()
#removing empty list elements
for i in range(len(simple2.iloc[0])):
    list_name = 'crate_' + str(i+1)
    while(" " in globals()[list_name]):
        globals()[list_name].remove(" ")
 
# Pushing & popping element on the top of the stack
    
def stack_push(stack1,stack2,count):
    for i in range(count):
        y = globals()[stack1][-1]
        globals()[stack2].append(y)
        globals()[stack1].pop()
          
# Displaying element on the top of the stack
def stack_peek(stack):
    element = globals()[stack][-1]
    print("Element on stack top :", element)

rearrange = pd.Series(day5[10:])
import re
pattern = r'\D+'
j = 0
values = []
while j < len(rearrange):
    values.append(re.split(pattern,rearrange[j]))
    j += 1
                  
    
#removing empty list elements
for i in range(len(values)):
    while("" in values[i]):
        values[i].remove("")
 
values2 = []
i = 0
while i <len(values):
    values2.append([int(x) for x in values[i]])
    i +=1

i = 0    
for val in range(len(values2)):
    crate1 = 'crate_' + str(values2[val][1])
    crate2 = 'crate_' + str(values2[val][2])
    count = values2[val][0]     
    stack_push(crate1,crate2,count)
    
for i in range(len(crates)):
    crate = 'crate_' + str(i+1)
    stack_peek(crate)
     
    
# Pushing & popping element on the top of the stack NEW
    
def stack_push_new(stack1,stack2,count):
    for i in range(count,0,-1):
        y = globals()[stack1][-i]
        globals()[stack2].append(y)
        globals()[stack1].pop(-i)
             
for val in range(len(values2)):
    crate1 = 'crate_' + str(values2[val][1])
    crate2 = 'crate_' + str(values2[val][2])
    count = values2[val][0]     
    stack_push_new(crate1,crate2,count)
    
for i in range(len(crates)):
    crate = 'crate_' + str(i+1)
    stack_peek(crate)
    
"""

Day 6

"""  
day6 = open('day6.txt', 'r').read().splitlines()
sample = ['mjqjpqmgbljsphdztnvjfqwrcgsmlb']
# sample find first 4 distinct count
i = 0
j = 4
marker = 0
while j < len(sample[0]):
    if(len(set(sample[0][i:j])) == len(sample[0][i:j])):
        marker = j
        break
    i += 1
    j += 1
#day6 find first 4 distinct count
i = 0
j = 4
marker = 0
while j < len(day6[0]):
    if(len(set(day6[0][i:j])) == len(day6[0][i:j])):
        marker = j
        break
    i += 1
    j += 1
# sample find first 14 distinct count    
i = 0
j = 14
marker = 0
while j < len(sample[0]):
    if(len(set(sample[0][i:j])) == len(sample[0][i:j])):
        marker = j
        break
    i += 1
    j += 1
# day6 find first 14 distinct count    
i = 0
j = 14
marker = 0
while j < len(day6[0]):
    if(len(set(day6[0][i:j])) == len(day6[0][i:j])):
        marker = j
        break
    i += 1
    j += 1
    
"""

Day 7

"""  
day7 = open('day7.txt', 'r').read().splitlines()
sample7 = open('sample7.txt', 'r').read().splitlines()

        


  
    