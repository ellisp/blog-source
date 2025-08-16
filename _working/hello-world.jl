

println("Hello world")
println(2+2)

println("basic for loop")
for i in 1:5
    print(i, ", ")
end

println()
println("for loop over an array")
a1 = [1,2,3,4]
for i in a1
    print(i, ", ")
end

using Plots

# plot some data
plot([cumsum(rand(500) .- 0.5), cumsum(rand(500) .- 0.5)])
