using Distributions

# Number of runs of simulation:
NR = 100

# Number of groups:
NG = 2

# Subjects per group:
NS = 20

# Number items in inventory:
NI = 100;



for r in 1:NR
    for g in 1:NG
        for s in 1:NS
            H = 0
            # Probability item performed with right hand:
            PR = ifelse(rand() < 0.2, 0.2, 0.9)
            for i in 1:NI
                if rand() < PR
                    H = H + 1
                end

            end
        end
    end
end    

