#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan  9 15:30:18 2023

@author: lhuynh
"""

from gurobipy import *

# Create a new model
m = Model("mip1")

# Create decision variables
c =m.addVar(vtype =GRB.CONTINUOUS, name="Chair")
d =m.addVar(vtype =GRB.CONTINUOUS, name="Desk")
t =m.addVar(vtype =GRB.CONTINUOUS, name="Table")

# Set objective function
m.setObjective(15*c+24*d+18*t, GRB.MAXIMIZE)

# Define & Add constraints
m.addConstr(4*c + 6*d + 2*t <= 1850, "c0")
m.addConstr(3*c + 5*d + 7*t <= 2400, "c1")
m.addConstr(3*c + 2*d + 4*t <= 1500, "c2")
m.addConstr(c <= 360, "c3")
m.addConstr(d <= 300, "c4")
m.addConstr(t <= 100, "c5")

# Tell it to optimize
m.optimize()

# Don't forget to print results
for v in m.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m.objVal )

# Slack calculation
for c in m.getConstrs():
    print("%s:%g"%(c.ConstrName,c.slack)) 
    
# Shadow prices/dual prices
shadow_prices = m.getAttr("Pi")
m.printAttr("Pi")

# Reduced costs
for v in m.getVars():
    print('%s %g' % (v.varName , v.rc ))

# Create a new model
m1 = Model("mip1")

# Create decision variables
s =m1.addVar(vtype =GRB.CONTINUOUS, name="Seeds")
r =m1.addVar(vtype =GRB.CONTINUOUS, name="Raisins")
f =m1.addVar(vtype =GRB.CONTINUOUS, name="Flakes")
p =m1.addVar(vtype =GRB.CONTINUOUS, name="Pecans")
w =m1.addVar(vtype =GRB.CONTINUOUS, name="Walnuts")

# Set objective function
m1.setObjective(4*s+ 5*r+ 3*f+ 7*p+ 6*w, GRB.MINIMIZE)

# Define & Add constraints
m1.addConstr(10*s+ 20*r+ 10*f+ 30*p+ 20*w >= 20, "c0")
m1.addConstr(5*s+ 7*r+ 4*f+ 9*p+ 2*w >= 10, "c1")
m1.addConstr(1*s+ 4*r+ 10*f+ 2*p+ 1*w >= 15, "c2")
m1.addConstr(500*s+ 450*r+ 160*f+ 300*p+ 500*w >= 600, "c3")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )

# Slack calculation
for c in m1.getConstrs():
    print("%s:%g"%(c.ConstrName,c.slack))

# Create a new model
m1 = Model("mip1")

# Create decision variables
b =m1.addVar(vtype =GRB.CONTINUOUS, name="Brazilian")
c =m1.addVar(vtype =GRB.CONTINUOUS, name="Colombian")
p =m1.addVar(vtype =GRB.CONTINUOUS, name="Peruvian")


# Set objective function
m1.setObjective(0.5*b + 0.6*c + 0.7*p, GRB.MINIMIZE)

# Define & Add constraints
m1.addConstr(b + c + p == 4000000, "Pounds")
m1.addConstr(-3*b - 18*c + 7*p >= 0, "Aroma")
m1.addConstr(-1*b + 4*c + 2*p >= 0, "Strength")
m1.addConstr(b <= 1500000, "c1")
m1.addConstr(c <= 1200000, "c2")
m1.addConstr(p <= 2000000, "c3")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )

# Slack calculation
for c in m1.getConstrs():
    print("%s:%g"%(c.ConstrName,c.slack))
    
# Shadow prices/dual prices
shadow_prices = m1.getAttr("Pi")
m1.printAttr("Pi")

# Reduced costs
for v in m1.getVars():
    print('%s %g' % (v.varName , v.rc ))
    
# Create a new model
m1 = Model("Marrs")

# Create decision variables
p1 =m1.addVar(vtype =GRB.BINARY, name="Info Syst")
p2 =m1.addVar(vtype =GRB.BINARY, name="New Tech")
p3 =m1.addVar(vtype =GRB.BINARY, name="Recycle")
p4 =m1.addVar(vtype =GRB.BINARY, name="Machine Center")
p5 =m1.addVar(vtype =GRB.BINARY, name="Receiving")


# Set objective function
m1.setObjective(10*p1 + 17*p2 + 16*p3 + 8*p4 + 14*p5, GRB.MAXIMIZE)

# Define & Add constraints
m1.addConstr(48*p1 + 96*p2 + 80*p3 + 32*p4 + 64*p5 <= 160, "Expenditure")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )
print(m1.status)

# Create a new model
m1 = Model("Set Covering")

# Create decision variables
s1 =m1.addVar(vtype =GRB.BINARY, name="S1")
s2 =m1.addVar(vtype =GRB.BINARY, name="S2")
s3 =m1.addVar(vtype =GRB.BINARY, name="S3")
s4 =m1.addVar(vtype =GRB.BINARY, name="S4")
s5 =m1.addVar(vtype =GRB.BINARY, name="S5")
s6 =m1.addVar(vtype =GRB.BINARY, name="S6")
s7 =m1.addVar(vtype =GRB.BINARY, name="S7")


# Set objective function
m1.setObjective(s1+s2+s3+s4+s5+s6+s7, GRB.MINIMIZE)

# Define & Add constraints
m1.addConstr(s2 +s4 +s7 >= 1, "District1")
m1.addConstr(s1 +s6 +s7 >= 1, "District2")
m1.addConstr(s2 +s6 +s7 >= 1, "District3")
m1.addConstr(s2 +s3 +s5 +s6 >= 1, "District4")
m1.addConstr(s1 +s3 +s5 >= 1, "District5")
m1.addConstr(s1 +s4 +s6 >= 1, "District6")
m1.addConstr(s1 +s7 >= 1, "District7")
m1.addConstr(s3 +s4 +s5 >= 1, "District8")
m1.addConstr(s1 +s5 >= 1, "District9")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
print(m1.status)
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )


# Use to find all available solutions
m1.setParam(GRB.Param.PoolSolutions,1024)
# pool gap allows solutions to be given to you # b/w 0 & 1
m1.setParam(GRB.Param.PoolGap,0.1) #gives anything w/in 10% of optimal solution
# Searches for more than one optimal solution
m1.setParam(GRB.Param.PoolSearchMode,2)

# Loop to print out all solutions
print("Number of solutions:",m1.SolCount)

print('Now showing all possible solutions and objective value')
for s in range(m1.SolCount):
    m1.setParam(GRB.Param.SolutionNumber,s)
    print("Objective value is",'%g'%m1.PoolObjVal)
    for q in m1.getVars():
        print('%s %g'%(q.varName,q.Xn))
        
# Create a new model
m1 = Model("Marrs")

# Create decision variables
p1 =m1.addVar(vtype =GRB.BINARY, name="Info Syst")
p2 =m1.addVar(vtype =GRB.BINARY, name="New Tech")
p3 =m1.addVar(vtype =GRB.BINARY, name="Recycle")
p4 =m1.addVar(vtype =GRB.BINARY, name="Machine Center")
p5 =m1.addVar(vtype =GRB.BINARY, name="Receiving")


# Set objective function
m1.setObjective(10*p1 + 17*p2 + 16*p3 + 8*p4 + 14*p5, GRB.MAXIMIZE)

# Define & Add constraints
m1.addConstr(48*p1 + 96*p2 + 80*p3 + 32*p4 + 64*p5 <= 160, "Expenditure")
m1.addConstr(p2 + p5 >= 1, "Restriction 1")
m1.addConstr(p1 + p2 + p3 + p4 + p5 <= 2, "Restriction 2")
m1.addConstr(p4 + p5 <= 1, "Restriction 3")
m1.addConstr(p3 - p5 >= 0, "Restriction 4")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )
print(m1.status)

# Create a new model
m1 = Model("Fixed Costs")

# Create decision variables
f1 =m1.addVar(vtype =GRB.CONTINUOUS, name="f1")
f2 =m1.addVar(vtype =GRB.CONTINUOUS, name="f2")
f3 =m1.addVar(vtype =GRB.CONTINUOUS, name="f3")
y1 =m1.addVar(vtype =GRB.BINARY, name="y1")
y2 =m1.addVar(vtype =GRB.BINARY, name="y2")
y3 =m1.addVar(vtype =GRB.BINARY, name="y3")


# Set objective function
m1.setObjective(1.2*f1 + 1.8*f2 + 2.2*f3 - 60*y1 - 200*y2 - 100*y3, GRB.MAXIMIZE)

# Define & Add constraints
m1.addConstr(3*f1 + 4 *f2 + 8*f3 <= 2000, "Dept A")
m1.addConstr(3*f1 + 5*f2 + 6*f3 <= 2000, "Dept B")
m1.addConstr(2*f1 + 3*f2 + 9*f3 <= 2000, "Dept C")
m1.addConstr(f1 - 400*y1 <= 0, "Fixed Cost 1")
m1.addConstr(f2 - 300*y2 <= 0, "Fixed Cost 2")
m1.addConstr(f3 - 50*y3 <= 0, "Fixed Cost 3")


# Tell it to optim1ize
m1.optimize()

# Don't forget to print results
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )
print(m1.status)

# Fixed costs example with vectorized code & loops
import numpy as np
coef_f = np.array([1.2,1.8,2.2])
coef_y=np.array([-60,-200,-100])
F_const = np.array([[3,4,8],[3,5,6],[2,3,9]])
Y_const = np.array([400,300,50])
m = Model("fixed costs")
dec_len=range(3)
F,y = {},{}
for j in dec_len:
    F[j] = m.addVar(obj=coef_f[j],vtype=GRB.CONTINUOUS,lb=0,name="F[%s]"%j)
for i in dec_len:
    y[i] = m.addVar(obj=coef_y[i],vtype=GRB.BINARY,name="y[%s]"%i)
m.ModelSense=GRB.MAXIMIZE
for i in range(F_const.shape[0]):
    m.addConstr((quicksum(F[j]*F_const[i][j] for j in dec_len))<= 2000,name="Dept[%s]"%i)
    m.addConstr(F[i]-(Y_const[i]*y[i])<= 0,name="Fixed[%s]"%i)
    
m.optimize()
print("Convergence status is ",m.status)

for v in m.getVars():
    print("%s%g"%(v.varName, v.x))
print("Obj: %g"%m.objVal)

# Create a new model
m = Model("Auto")

# Create decision variables
s =m.addVar(vtype =GRB.CONTINUOUS, name="Subcompact")
c =m.addVar(vtype =GRB.CONTINUOUS, name="Compact")
i =m.addVar(vtype =GRB.CONTINUOUS, name="Intermediate")
l =m.addVar(vtype =GRB.CONTINUOUS, name="Luxury")
t =m.addVar(vtype =GRB.CONTINUOUS, name="Truck")
v =m.addVar(vtype =GRB.CONTINUOUS, name="Van")

# Set objective function
m.setObjective(150*s + 225*c + 250*i + 500*l + 400*t + 200*v, GRB.MAXIMIZE)

# Define & Add constraints
m.addConstr(s + c + i + l + t + v <= 1200000, "annual capacity")
m.addConstr(s + c >= 510000, "S & C capacity 50%")
m.addConstr(s + c <= 620000, "S & C")
m.addConstr(i + l <= 400000, "I & L")
m.addConstr(t + v <= 275000, "T & V")
m.addConstr(13*s + 7*c - 12*i - 15*l - 7*t - 2*v  >= 0, "fuel economy")
m.addConstr(s <= 600000, "s")
m.addConstr(c <= 400000, "c")
m.addConstr(i <= 300000, "i")
m.addConstr(l <= 225000, "l")
m.addConstr(t <= 325000, "t")
m.addConstr(v <= 100000, "v")

# Tell it to optimize
m.optimize()
print("Convergence status is ",m.status)

# Don't forget to print results
for v in m.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m.objVal )

# Slack calculation
for c in m.getConstrs():
    print("%s:%g"%(c.ConstrName,c.slack)) 
    
# Shadow prices/dual prices
shadow_prices = m.getAttr("Pi")
m.printAttr("Pi")

# Reduced costs
for v in m.getVars():
    print('%s %g' % (v.varName , v.rc ))

m1 = Model('Electronics')
# Create variables
P1 = m1.addVar(vtype=GRB.BINARY, name="P1")
P2 = m1.addVar(vtype=GRB.BINARY, name="P2")
P3 = m1.addVar(vtype=GRB.BINARY, name="P3")
P4 = m1.addVar(vtype=GRB.BINARY, name="P4")
P5 = m1.addVar(vtype=GRB.BINARY, name="P5")
P6 = m1.addVar(vtype=GRB.BINARY, name="P6")
P7 = m1.addVar(vtype=GRB.BINARY, name="P7")
P8 = m1.addVar(vtype=GRB.BINARY, name="P8")
# Set objective
m1.setObjective(36000*P1 + 82000*P2 + 29000*P3 + 16000*P4 + 56000*P5 + 61000*P6 + 48000*P7 + 41000*P8,GRB.MAXIMIZE)
# Add constraints
m1.addConstr(60000*P1 + 110000*P2 + 53000*P3 + 47000*P4 + 92000*P5 + 85000*P6 + 73000*P7 + 65000*P8 <= 300000, "EXPENSE")
m1.addConstr(7*P1 + 9*P2 + 8*P3 + 4*P4 + 7*P5 + 6*P6 + 8*P7 + 5*P8 <= 40, "PERSONNEL")
m1.optimize()
print("Convergence status is ",m1.status)

# Don't forget to print results
vars = m1.getVars()
for v in m1.getVars():
    print('%s %g' % (v.varName , v.x ))
print('Obj : %g' % m1.objVal )

# Number of solutions
print('Number of solutions:',m1.SolCount)

# Slack calculation
for c in m1.getConstrs():
    print("%s:%g"%(c.ConstrName,c.slack)) 
    
# Shadow prices/dual prices
shadow_prices = m1.getAttr("Pi")
m1.printAttr("Pi")

# Reduced costs
for v in m1.getVars():
    print('%s %g' % (v.varName , v.rc ))
    
# Use to find all available solutions
m1.setParam(GRB.Param.PoolSolutions,1024)
# pool gap allows solutions to be given to you # b/w 0 & 1
m1.setParam(GRB.Param.PoolGap,0.1) #gives anything w/in 10% of optimal solution
# Searches for more than one optimal solution
m1.setParam(GRB.Param.PoolSearchMode,2)

# Loop to print out all solutions
print("Number of solutions:",m1.SolCount)

print('Now showing all possible solutions and objective value')
for s in range(m1.SolCount):
    m1.setParam(GRB.Param.SolutionNumber,s)
    print("Objective value is",'%g'%m1.PoolObjVal)
    for q in m1.getVars():
        print('%s %g'%(q.varName,q.Xn))