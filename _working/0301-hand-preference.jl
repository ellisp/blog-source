using Distributions

# Number of runs of simulation:
NR = 100

# Number of groups:
NG = 2

# Subjects per group:
NS = 50

# Number items in inventory:
NI = 5;


# An array of subject scores. This wasn't defined in the original 
# program
results = Array{Int64}(undef, NG, NS, NR)

println("Starting handedness experiment simulation")
println("with ", NR, " runs of a simulation with ", NG, " groups ")
println("of ", NS, " subjects, each tested with ", NI, " items.")
println()

for r in 1:NR
    for g in 1:NG
        for s in 1:NS

            # Count of performances with right hand for this subject:
            h = 0 
            # Probability this subject performs item with right hand:
            PR = ifelse(rand() < 0.2, 0.2, 0.9)

            # Now for each subject, we are going to count the number
            # of items picked up by right hand

            for i in 1:NI
                if rand() < PR
                    h = h + 1
                end
            end
            # original was N(G,H)=N(G,H)+1.This just seems wrong to me.
            results[g, s, r] = h
        end
    end
end    

display(results)

