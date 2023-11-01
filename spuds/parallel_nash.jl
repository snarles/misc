using Random
using DataFrames
using CSV
using DelimitedFiles
using Statistics
using Printf
using LinearAlgebra
using Dates

struct Spud
    name::String
    h::Int64
    f::Int64
    l::Int64
    p::Int64
    r::Int64
    s::Int64
end

const MXS = 100
const MXV = 10
const MNV = 1

function cost(h::Int64, f::Int64, l::Int64, p::Int64, r::Int64, s::Int64)::Int64
    #return h*(f+l-2*MNV) + f*(p+r+s-3*MNV) + h+f+l+p+r+s-6*MNV
    return h*(f+l) + f*(p+r+s)
end

function compare_int_list(as::Vector{Int64}, bs::Vector{Int64}, tiebreaker::Int64 = 0)::Int64
    n = min(length(as), length(bs))
    for i in 1:n
        if as[i] != bs[i]
            return sign(as[i] - bs[i])
        end
    end
    return tiebreaker
end

function spud_h_seq(a::Spud)::Vector{Int64}
    return [a.h, a.s, a.r, a.p, a.l, a.f]
end

function spud_f_seq(a::Spud)::Vector{Int64}
    return [a.f, a.s, a.r, a.p, a.l, a.h]
end

function spud_l_seq(a::Spud)::Vector{Int64}
    return [a.l]
end

function spud_p_seq(a::Spud)::Vector{Int64}
    return [a.p, a.l]
end

function spud_r_seq(a::Spud)::Vector{Int64}
    return [a.r, a.f]
end

function spud_s_seq(a::Spud)::Vector{Int64}
    return [a.s, a.h]
end

function spud_utb_seq(a::Spud)::Vector{Int64}
    return [a.h, a.f, a.l, a.p, a.r, a.s]
end

function eval_finds(a::Spud, b::Spud, tiebreaker::Int64 = 0)::Int64
    ev = compare_int_list(spud_f_seq(a), spud_h_seq(b), tiebreaker)
    ans = ev
end

function eval_melee(a::Spud, b::Spud, tiebreaker1::Int64 = 0, tiebreaker2::Int64 = 0)::Int64
    comp_p = compare_int_list(spud_p_seq(a), spud_p_seq(b), tiebreaker1)
    comp_r = compare_int_list(spud_r_seq(a), spud_r_seq(b), tiebreaker1)
    comp_s = compare_int_list(spud_s_seq(a), spud_s_seq(b), tiebreaker1)
    ev = 4 * comp_p + 3 * comp_r + 2 * comp_s
    return sign(ev + (1-abs(ev))*tiebreaker2)
end

function eval_battle(a::Spud, b::Spud)::Int64
    utb = compare_int_list(spud_utb_seq(a), spud_utb_seq(b), 0) # universal tiebreaker
    if utb == 0
        return 0
    end
    a_finds = eval_finds(a, b, utb)==1
    b_finds = eval_finds(b, a, utb)==1
    melee_win = eval_melee(a, b, 0, utb)
    if a_finds && b_finds
        return melee_win
    end
    if a_finds && !b_finds
        return 1
    end
    if !a_finds && b_finds
        return -1
    end
    if !a_finds && !b_finds
        return compare_int_list([a.l, melee_win], [b.l, -melee_win], 0)
    end
end

tab = CSV.read("census_yob2022_names.txt", DataFrame, header = false)
names = tab.Column1
adjectives = CSV.read("adjectives.csv", DataFrame)
nouns = CSV.read("nouns.csv", DataFrame)
jobs = CSV.read("jobs.csv", DataFrame)

const mult_noun = 1
const mult_adj = 2
const mult_job = 3


function random_name_and_stat()::Spud
    vp = [0, 0, 0, 0, 0, 0]
    #nametype = rand([1,1,1,1,1,2,2,2,3])
    name = ""
    noun = ""
    adj = ""
    noun_i = rand(1:nrow(nouns))
    noun = nouns[noun_i, :noun]
    vp[1] = vp[1] + nouns[noun_i, :H] * mult_noun
    vp[2] = vp[2] + nouns[noun_i, :F] * mult_noun
    vp[3] = vp[3] + nouns[noun_i, :L] * mult_noun
    vp[4] = vp[4] + nouns[noun_i, :P] * mult_noun
    vp[5] = vp[5] + nouns[noun_i, :R] * mult_noun
    vp[6] = vp[6] + nouns[noun_i, :S] * mult_noun
    adj_i = rand(1:nrow(adjectives))
    adj = adjectives[adj_i, :adjective]
    vp[1] = vp[1] + adjectives[adj_i, :H] * mult_adj
    vp[2] = vp[2] + adjectives[adj_i, :F] * mult_adj
    vp[3] = vp[3] + adjectives[adj_i, :L] * mult_adj
    vp[4] = vp[4] + adjectives[adj_i, :P] * mult_adj
    vp[5] = vp[5] + adjectives[adj_i, :R] * mult_adj
    vp[6] = vp[6] + adjectives[adj_i, :S] * mult_adj
    job_i = rand(1:nrow(jobs))
    job = jobs[job_i, :job]
    vp[1] = vp[1] + jobs[job_i, :H] * mult_job
    vp[2] = vp[2] + jobs[job_i, :F] * mult_job
    vp[3] = vp[3] + jobs[job_i, :L] * mult_job
    vp[4] = vp[4] + jobs[job_i, :P] * mult_job
    vp[5] = vp[5] + jobs[job_i, :R] * mult_job
    vp[6] = vp[6] + jobs[job_i, :S] * mult_job
    name = string(adj, " ", job, " ", noun)
    Spud(name, vp[1], vp[2], vp[3], vp[4], vp[5], vp[6])
end

function rand_rename(a::Spud, n_tries::Int = 100)::Spud
    best_score = 0.0
    best_b = random_name_and_stat()
    for ii in 1:n_tries
        b = random_name_and_stat()
        b_norm = sqrt(b.h^2 + b.f^2 + b.l^2 + b.p^2 + b.r^2 + b.s^2)
        score = (a.h * b.h + a.f * b.f + a.l * b.l + a.p * b.p + a.r * b.r + a.s * b.s)/b_norm
        if score > best_score
            best_score = score
            best_b = b
        end
    end
    return Spud(best_b.name, a.h, a.f, a.l, a.p, a.r, a.s)
end

function eval_battle_list(a::Spud, bs::Array{Spud})::Int
    score = 0
    for ii in 1:length(bs)
        score = score + eval_battle(a, bs[ii])
    end
    return score
end

function eval_battle_list2(a::Spud, bs::Array{Spud}, w::Vector{Float64})::AbstractFloat
    score = 0.0
    for ii in 1:length(bs)
        score = score + w[ii] * eval_battle(a, bs[ii])
    end
    return score
end


function pick_best(as::Array{Spud}, bs::Array{Spud})::Spud
    bestscore = -999
    bestf = as[1]
    for ii in 1:length(as)
        score = eval_battle_list(as[ii], bs)
        if score > bestscore
            bestscore = score
            bestf = as[ii]
        end
    end
    return bestf
end

function pick_best_rdmly(as::Array{Spud}, bs::Array{Spud}, ntries::Int)::Spud
    bestscore = -999
    bestf = rand(as)
    for ii in 1:ntries
        f = rand(as)
        score = eval_battle_list(f, bs)
        if score > bestscore
            bestscore = score
            bestf = f
        end
    end
    return bestf
end

function pick_best_rdmly_g(g::Function, bs::Array{Spud}, ntries::Int)::Spud
    bestscore = -999
    bestf = g()
    for ii in 1:ntries
        f = g()
        score = eval_battle_list(f, bs)
        if score > bestscore
            bestscore = score
            bestf = f
        end
    end
    return bestf
end

function spuds_to_df(as::Array{Spud})::DataFrame
    names = Array{String}(undef, length(as))
    hs = Array{Int}(undef, length(as))
    fs = Array{Int}(undef, length(as))
    ls = Array{Int}(undef, length(as))
    ps = Array{Int}(undef, length(as))
    rs = Array{Int}(undef, length(as))
    ss = Array{Int}(undef, length(as))
    for ii in 1:length(as)
        names[ii] = as[ii].name
        hs[ii] = as[ii].h
        fs[ii] = as[ii].f
        ls[ii] = as[ii].l
        ps[ii] = as[ii].p
        rs[ii] = as[ii].r
        ss[ii] = as[ii].s
    end
    df = DataFrame(name = names, h = hs, f = fs, l = ls, p = ps, r = rs, s = ss)
    return df
end

function fpart(x::AbstractFloat)::AbstractFloat
  return x - trunc(x)
end

function eval_team_battle(as::Array{Spud}, bs::Array{Spud})::Int
    a_i = 1
    b_i = 1
    while (a_i <= length(as)) && (b_i <= length(bs))
        res = eval_battle(as[a_i], bs[b_i])
        if res == 1
            b_i = b_i + 1
        else
            a_i = a_i + 1
            if res == 0
                b_i = b_i + 1
            end
        end
    end
    a_out = (a_i > length(as))
    b_out = (b_i > length(as))
    if a_out
        if b_out
            return 0
        else
            return -1
        end
    else
        return 1
    end
end

function compare_generator(f1, f2, limit)
    a_i = 1
    b_i = 1
    f_a = f1()
    f_b = f2()
    while (a_i < limit) && (b_i < limit)
        res = eval_battle(f_a, f_b)
        if res != -1
            b_i = b_i + 1
            f_b = f2()
        end
        if res != 1
            a_i = a_i + 1
            f_a = f1()
        end
    end
    return (a_i/limit, b_i/limit)
end

function random_team(f::Function, team_size::Int)::Array{Spud}
    team = Array{Spud}(undef, team_size)
    for i in 1:team_size
        team[i] = f()
    end
    return team
end

function df_to_spuds(df::DataFrame)::Array{Spud}
    n = size(df)[1]
    as = Array{Spud}(undef, n)
    for i in 1:n
        as[i] = Spud(df[i, :name], df[i, :h], df[i, :f], df[i, :l], df[i, :p], df[i, :r], df[i, :s])
    end
    return as
end

function cost(a::Spud)::Int64
    return cost(a.h, a.f, a.l, a.p, a.r, a.s)
end

function upgrade_spud(sp::Spud)::Spud
    h = sp.h
    f = sp.f
    l = sp.l
    p = sp.p
    r = sp.r
    s = sp.s    
    check_h = (h == MXV) || (cost(h+1,f,l,p,r,s) > MXS)
    check_f = (f == MXV) || (cost(h,f+1,l,p,r,s) > MXS)
    check_l = (l == MXV) || (cost(h,f,l+1,p,r,s) > MXS)
    check_prs = (p+r+s == 3*MXV) || (cost(h,f,l,p+1,r,s) > MXS)
    while !(check_h && check_f && check_l && check_prs)
        rand_i = rand(1:4)
        if rand_i == 1 && !check_h
            h = h+1
        end
        if rand_i == 2 && !check_f
            f = f+1
        end
        if rand_i == 3 && !check_l
            l = l+1
        end
        if rand_i == 4 && !check_prs
            rand_j = rand([1,1,1,1,1,2,2,2,2,3,3,3])
            if rand_j == 1 && p < MXV
                p += 1
            end
            if rand_j == 2 && r < MXV
                r += 1
            end
            if rand_j == 3 && s < MXV
                s += 1
            end
        end            
        check_h = (h == MXV) || (cost(h+1,f,l,p,r,s) > MXS)
        check_f = (f == MXV) || (cost(h,f+1,l,p,r,s) > MXS)
        check_l = (l == MXV) || (cost(h,f,l+1,p,r,s) > MXS)
        check_prs = (p+r+s == 3*MXV) || (cost(h,f,l,p+1,r,s) > MXS)
    end
    return Spud(sp.name, h, f, l, p, r, s)
end



function pick_best_library(bs::Array{Spud})::Spud
    bestscore = -999
    bestf = Spud("",MNV,MNV,MNV,MNV,MNV,MNV)
    df = spuds_to_df(bs)
    hrange = vcat([MNV], df.h, df.h .+ 1, df.f, df.f .+ 1)
    frange = vcat([MNV], df.f, df.f .+ 1, df.h, df.h .+ 1)
    lrange = vcat([MNV], df.l, df.l .+ 1)
    prange = vcat([MNV], df.p, df.p .+ 1)
    rrange = vcat([MNV], df.r, df.r .+ 1)
    srange = vcat([MNV], df.s, df.s .+ 1)
    hrange = sort(unique(hrange))
    frange = sort(unique(frange))
    lrange = sort(unique(lrange))
    prange = sort(unique(prange))
    rrange = sort(unique(rrange))
    srange = sort(unique(srange))
    hrange = hrange[hrange .<= MXV]
    frange = frange[frange .<= MXV]
    lrange = lrange[lrange .<= MXV]
    prange = prange[prange .<= MXV]
    rrange = rrange[rrange .<= MXV]
    srange = srange[srange .<= MXV]
    for h in hrange
        if (cost(h, MNV, MNV, MNV, MNV, MNV) <= MXS)
            for f in frange
                if (cost(h, f, MNV, MNV, MNV, MNV) <= MXS)
                    for l in lrange
                        if (cost(h, f, l, MNV, MNV, MNV) <= MXS)
                            for p in prange
                                if (cost(h, f, l, p, MNV, MNV) <= MXS)
                                    for r in rrange
                                        if (cost(h, f, l, p, r, MNV) <= MXS)
                                            for s in srange
                                                if (cost(h,f,l,p,r,s) <= MXS)
                                                    ff = Spud("",h,f,l,p,r,s)
                                                    score = eval_battle_list(ff, bs)
                                                    if score > bestscore
                                                        bestscore = score
                                                        bestf = ff
                                                    end
                                                end
                                            end
                                        end
                                    end                        
                                end
                            end                        
                        end
                    end
                end
            end
        end
    end
    return upgrade_spud(bestf)
end

function pick_ok_counter(bs::Array{Spud})::Spud
    bestscore = -999
    bestf = Spud("",MNV,MNV,MNV,MNV,MNV,MNV)
    df = spuds_to_df(bs)
    hrange = vcat([MNV], df.f .+ 1)
    frange = vcat([MNV], df.h .+ 1)
    lrange = vcat([MNV], df.l .+ 1)
    prange = vcat([MNV], df.p .+ 1)
    rrange = vcat([MNV], df.r .+ 1)
    srange = vcat([MNV], df.s .+ 1)
    hrange = sort(unique(hrange))
    frange = sort(unique(frange))
    lrange = sort(unique(lrange))
    prange = sort(unique(prange))
    rrange = sort(unique(rrange))
    srange = sort(unique(srange))
    hrange = hrange[hrange .<= MXV]
    frange = frange[frange .<= MXV]
    lrange = lrange[lrange .<= MXV]
    prange = prange[prange .<= MXV]
    rrange = rrange[rrange .<= MXV]
    srange = srange[srange .<= MXV]
    for h in hrange
        if (cost(h, MNV, MNV, MNV, MNV, MNV) <= MXS)
            for f in frange
                if (cost(h, f, MNV, MNV, MNV, MNV) <= MXS)
                    for l in lrange
                        if (cost(h, f, l, MNV, MNV, MNV) <= MXS)
                            for p in prange
                                if (cost(h, f, l, p, MNV, MNV) <= MXS)
                                    for r in rrange
                                        if (cost(h, f, l, p, r, MNV) <= MXS)
                                            for s in srange
                                                if (cost(h,f,l,p,r,s) <= MXS)
                                                    ff = Spud("",h,f,l,p,r,s)
                                                    score = eval_battle_list(ff, bs)
                                                    if score > bestscore
                                                        bestscore = score
                                                        bestf = ff
                                                    end
                                                end
                                            end
                                        end
                                    end                        
                                end
                            end                        
                        end
                    end
                end
            end
        end
    end
    return upgrade_spud(bestf)
end

function pick_best_library2(bs::Array{Spud}, w::Vector{Float64})::Spud
    bestscore = -999.9
    bestf = Spud("",MNV,MNV,MNV,MNV,MNV,MNV)
    df = spuds_to_df(bs)
    hrange = vcat([MNV], df.h, df.h .+ 1, df.f, df.f .+ 1)
    frange = vcat([MNV], df.f, df.f .+ 1, df.h, df.h .+ 1)
    lrange = vcat([MNV], df.l, df.l .+ 1)
    prange = vcat([MNV], df.p, df.p .+ 1)
    rrange = vcat([MNV], df.r, df.r .+ 1)
    srange = vcat([MNV], df.s, df.s .+ 1)
    hrange = sort(unique(hrange))
    frange = sort(unique(frange))
    lrange = sort(unique(lrange))
    prange = sort(unique(prange))
    rrange = sort(unique(rrange))
    srange = sort(unique(srange))
    hrange = hrange[hrange .<= MXV]
    frange = frange[frange .<= MXV]
    lrange = lrange[lrange .<= MXV]
    prange = prange[prange .<= MXV]
    rrange = rrange[rrange .<= MXV]
    srange = srange[srange .<= MXV]
    for h in hrange
        if (cost(h, MNV, MNV, MNV, MNV, MNV) <= MXS)
            for f in frange
                if (cost(h, f, MNV, MNV, MNV, MNV) <= MXS)
                    for l in lrange
                        if (cost(h, f, l, MNV, MNV, MNV) <= MXS)
                            for p in prange
                                if (cost(h, f, l, p, MNV, MNV) <= MXS)
                                    for r in rrange
                                        if (cost(h, f, l, p, r, MNV) <= MXS)
                                            for s in srange
                                                if (cost(h,f,l,p,r,s) <= MXS)
                                                    ff = Spud("",h,f,l,p,r,s)
                                                    score = eval_battle_list2(ff, bs, w) + 0.00001 * rand()
                                                    if score > bestscore
                                                        bestscore = score
                                                        bestf = ff
                                                    end
                                                end
                                            end
                                        end
                                    end                        
                                end
                            end                        
                        end
                    end
                end
            end
        end
    end
    return upgrade_spud(bestf)
end

function subset_library(bs::Array{Spud}, n_hits::Int, w::Vector{Float64}, thres::Float64)::Array{Spud}
    bestf = Array{Spud}(undef, n_hits)
    evs = [-9.9 for i in 1:n_hits]
    spud_i = 1
    prev_spud_i = spud_i
    df = spuds_to_df(bs)
    best_score = 0.0
    hrange = vcat([MNV], df.h, df.h .+ 1, df.f, df.f .+ 1)
    frange = vcat([MNV], df.f, df.f .+ 1, df.h, df.h .+ 1)
    lrange = vcat([MNV], df.l, df.l .+ 1)
    prange = vcat([MNV], df.p, df.p .+ 1)
    rrange = vcat([MNV], df.r, df.r .+ 1)
    srange = vcat([MNV], df.s, df.s .+ 1)
    hrange = sort(unique(hrange))
    frange = sort(unique(frange))
    lrange = sort(unique(lrange))
    prange = sort(unique(prange))
    rrange = sort(unique(rrange))
    srange = sort(unique(srange))
    hrange = hrange[hrange .<= MXV]
    frange = frange[frange .<= MXV]
    lrange = lrange[lrange .<= MXV]
    prange = prange[prange .<= MXV]
    rrange = rrange[rrange .<= MXV]
    srange = srange[srange .<= MXV]
    overflow = 0
    for h in hrange
        if (cost(h, MNV, MNV, MNV, MNV, MNV) <= MXS)
            for f in frange
                if (cost(h, f, MNV, MNV, MNV, MNV) <= MXS)
                    for l in lrange
                        if (cost(h, f, l, MNV, MNV, MNV) <= MXS)
                            for p in prange
                                if (cost(h, f, l, p, MNV, MNV) <= MXS)
                                    for r in rrange
                                        if (cost(h, f, l, p, r, MNV) <= MXS)
                                            for s in srange
                                                if (cost(h,f,l,p,r,s) <= MXS)
                                                    ff = Spud("",h,f,l,p,r,s)
                                                    score = eval_battle_list2(ff, bs, w)
                                                    if score > best_score
                                                        best_score = score
                                                    end
                                                    if score >= (best_score - thres)
                                                        # find the first spud_i with ev less than best_score - thres
                                                        if spud_i > n_hits
                                                            spud_i = 1
                                                            overflow = 1
                                                        end
                                                        ntries = 0
                                                        while (evs[spud_i] >= best_score - thres)
                                                            spud_i += 1
                                                            ntries += 1
                                                            if spud_i > n_hits
                                                                spud_i = 1
                                                                overflow = 1
                                                            end
                                                            if ntries > n_hits
                                                                println("Full buffer")
                                                                #println(best_score)
                                                                #println(thres)
                                                                #println(evs)
                                                                if overflow==1
                                                                    return unique(bestf[evs .>= best_score - thres])
                                                                else
                                                                    bestf = bestf[1:prev_spud_i]
                                                                    evs = evs[1:prev_spud_i]
                                                                    return unique(bestf[evs .>= best_score - thres])
                                                                end
                                                            end
                                                        end
                                                        bestf[spud_i] = upgrade_spud(ff)
                                                        evs[spud_i] = score
                                                        prev_spud_i = spud_i
                                                        spud_i += 1
                                                    end
                                                end
                                            end
                                        end
                                    end                        
                                end
                            end                        
                        end
                    end
                end
            end
        end
    end
    #println(best_score)
    if overflow==1
        return unique(bestf[evs .>= best_score - thres])
    else
        bestf = bestf[1:prev_spud_i]
        evs = evs[1:prev_spud_i]
        return unique(bestf[evs .>= best_score - thres])
    end
end

# form initial library by subsampling indices
library = Array{Spud}(undef, 100000)
global spud_i = 0
ss_prob = 1.1



hrange = MNV:MXV
frange = MNV:MXV
lrange = MNV:MXV
prange = MNV:MXV
rrange = MNV:MXV
srange = MNV:MXV

for h in hrange
    if (cost(h, MNV, MNV, MNV, MNV, MNV) <= MXS)
        for f in frange
            if (cost(h, f, MNV, MNV, MNV, MNV) <= MXS)
                for l in lrange
                    if (cost(h, f, l, MNV, MNV, MNV) <= MXS)
                        for p in prange
                            if (cost(h, f, l, p, MNV, MNV) <= MXS)
                                for r in rrange
                                    if (cost(h, f, l, p, r, MNV) <= MXS)
                                        for s in srange
                                            if rand() < ss_prob && (cost(h,f,l,p,r,s) <= MXS)
                                                check_f = (h == MXV) || (cost(h+1,f,l,p,r,s) > MXS)
                                                check_h = (f == MXV) || (cost(h,f+1,l,p,r,s) > MXS)
                                                check_l = (l == MXV) || (cost(h,f,l+1,p,r,s) > MXS)
                                                check_prs = (p+r+s == 3*MXV) || (cost(h,f,l,p+1,r,s) > MXS)
                                                if check_h && check_f && check_l && check_prs
                                                    global spud_i += 1
                                                    #randname = rand_rename(Spud(" ",h,f,l,p,r,s)).name
                                                    #name = string("#", @sprintf("%i", spud_i), ". ", randname)
                                                    name = ""
                                                    library[spud_i] = Spud(name,h,f,l,p,r,s)
                                                end
                                            end
                                        end
                                    end
                                end                        
                            end
                        end                        
                    end
                end
            end
        end
    end
end

library = unique(library[1:spud_i])
n_spuds = length(library)

function ffp(nash_env, nits)
    n_nash = length(nash_env)
    i_lose = Array{Int}(undef, (n_nash, n_nash))
    n_lose = Array{Int}(undef, n_nash)
    for i in 1:n_nash
        n_lose[i] = 0
        ff = nash_env[i]
        for j in 1:n_nash
            if eval_battle(ff, nash_env[j]) ==-1
                n_lose[i] += 1
                i_lose[i, n_lose[i]] = j
            end
        end
    end
    counts = [0 for i in 1:n_nash]
    wins = [0 for i in 1:n_nash]
    for iter in 1:nits
        wc = 2 .* wins .+ counts
        ind_winners = findall(wc .== maximum(wc))
        i = rand(ind_winners)
        counts[i] += 1
        for j in 1:n_lose[i]
            i_w = i_lose[i, j]
            wins[i_w]+= 1
        end
    end
    return counts
end


nash_env = library[1:10];
#@time counts = ffp(nash_env, 1000);

@time counts = ffp(nash_env, 100000000);

nash_env2 = nash_env[counts .> 1]
counts3 = counts[counts .> 1]
for i in 1:length(nash_env2)
    ff = nash_env2[i]
    nash_env2[i] = Spud(string("c", counts3[i]), ff.h, ff.f, ff.l, ff.p, ff.r, ff.s)
end


CSV.write(@sprintf("spudsDpar/spudsD_%i.csv", rand(1000:1999)), spuds_to_df(nash_env2[sortperm(-counts3)]))
