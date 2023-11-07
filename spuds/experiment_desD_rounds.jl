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
    a1::Int64
end

const ability_none = 999
const ability_imitative = 10
const ability_reciprocating = 20
const ability_romantic = 30
const ability_bibliophile = 40
const ability_melodious = 50
const ability_iconoclast = 90
const ability_intoxicating = 100
const ability_otaku = 110
const ability_sincere = 120
const ability_cowardly = 130


ability_name = Dict(
    ability_imitative => "Imitative*",
    ability_reciprocating => "Reciprocating*",
    ability_romantic => "Romantic*",
    ability_bibliophile => "Bibliophile*",
    ability_melodious => "Melodious*",
    ability_iconoclast => "Iconoclast*",
    ability_intoxicating => "Intoxicating*",
    ability_otaku => "Otaku*",
    ability_sincere => "Sincere*",
    ability_cowardly => "Cowardly*",
)

function cost_imitative(a::Spud)
    h_inc = [4, 4, 3, 2, 2, 1, 1, 1, 0, 0] # how much H effectively increases given base H
    c =  6 + h_inc[a.h] * (a.f + a.l) + 2 * ifelse(a.l >= 8, 1, 0)
    if a.f >= 9 && a.l >= 4
        c += 3
    end
    if a.l == 10 && a.p == 10
        c += 8
    end
    return c
end

function fx_imitative(a::Spud, b::Spud)
    if b.h > a.h
        a = Spud(a.name, b.h, a.f, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function cost_reciprocating(a::Spud)
    # how much F effectively increases given base F (times 2)
    f_inc = [8, 6, 4, 2, 1, 1, 1, 1, 1, 1]
    c = 3 + f_inc[a.f] * div(a.h + a.p + a.r + a.s, 2)
    c = c + ifelse(a.h >= 9, 1, 0) + ifelse(a.p + a.r + a.s >= 16 && a.l >= 10, 4, 0)
    return c
end

function fx_reciprocating(a::Spud, b::Spud)
    if b.f > a.f
        a = Spud(a.name, a.h, b.f, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function cost_romantic(a::Spud)
    # how much L effectively increases given base L
    l_inc = [3, 2, 2, 1, 1, 1, 1, 0, 0, 0]
    c = 5 + l_inc[a.l] * a.h * ifelse(a.h >= 8, 2, 1)
    # nerf Spud("", 9, 1, 4, 10, 10, 10, 999)
    if a.h >= 9 && (a.p + a.r + a.s) >= 30
        c += 3
    end
    return c
end

function fx_romantic(a::Spud, b::Spud)
    if b.p > a.l
        a = Spud(a.name, a.h, a.f, b.p, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function cost_bibliophile(a::Spud)
    # ban full p/r/s
    if a.p + a.r + a.s >= 30
        return 100
    end
    # how much L effectively increases given base L
    l_inc = [2, 2, 2, 1, 1, 1, 1, 0, 0, 0];
    # for penalizing large H
    # h_inc = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    c = 4 + l_inc[a.l] * a.h
    return c
end

function fx_bibliophile(a::Spud, b::Spud)
    if b.r > a.l
        a = Spud(a.name, a.h, a.f, b.r, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function cost_melodious(a::Spud)
    h_inc = [0,0,0,0,0,1,1,2,4,10]
    f_inc = [0,0,0,0,1,1,4,5,9,11]
    c = 5 + f_inc[a.f] + 2 * h_inc[a.h] + ifelse(a.r >= 10, 3, 0)
    # nerf Spud("", 3, 9, 1, 3, 2, 1, 999)
    c += ifelse(a.f >= 9 && (a.p * a.h) >= 9, 3, 0)
    return c
end

function fx_melodious(a::Spud, b::Spud)
    if b.p > b.r && b.p > b.s
        a = Spud(a.name, a.h, a.f, a.r, a.p + 3, a.r, a.s, a.a1)
    end
    return [a, b]
end

function cost_iconoclast(a::Spud)
    # ban 10's
    if sum([a.h,a.f,a.l,a.p,a.r,a.s].==10) > 0
        return 100
    end
    # nerf Spud("", 2, 5, 2, 2, 3, 9, 999)
    c = 0
    if a.h == 2 && a.f == 5 && a.l == 2 && a.p ==2 && a.r==3 && a.s == 9
        c += 1
    end
    l_inc = [0,0,0,0,0,0,0,3,7]
    s_inc = [1,2,3,3,3,3,3,3,3]
    c = 13 + (3 * l_inc[a.l] + 1) * (s_inc[a.s])
    return c
end


function fx_iconoclast(a::Spud, b::Spud)
    v = sum([b.h,b.f,b.l,b.p,b.r,b.s] .== 10)
    if v >= 3
        aa = [a.h, a.f, a.l, a.p, a.r, a.s]
        aa2 = map(x -> div(x, 4), aa)
        r = aa .+ aa2
        a = Spud(a.name, r[1], r[2], r[3], r[4], r[5], r[6], a.a1)
    end
    return [a, b]
end

#df[:, :c2] = df.cost .+ (df.f .+ l_inc[df.l]) .* map(x -> div(x, 5), 4 .+ df.p .+ df.r .+ df.s) .+ 3;
function cost_intoxicating(a::Spud)
    l_inc = [0,0,0,0,0, 0,0,1,2,3]
    c = 3 + (a.f + l_inc[a.l]) * div(4 + a.p + a.r + a.s, 5)
    # nerf Spud("", 1, 4, 10, 2, 10, 3, 999)
    if a.h == 1 && a.f == 4 && a.l == 10 && a.p == 2 && a.r == 10 && a.s == 3
        c += 3
    end
    return c
end

function fx_intoxicating(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f, b.l, b.p, b.r, b.f, b.a1)
    return [a, b]
end

function cost_otaku(a::Spud)
    l_inc = [0,0,0,0,0, 0,0,0,0,8]
    c = 0 + l_inc[a.l] * div(7 + a.p + a.r + a.s, 8)
    if maximum([a.p, a.r, a.s]) == 10
        c += a.l
    end
    if c < 6
        c = 6
    end
    return c
end

function fx_otaku(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f, b.l, b.p, b.h, b.s, b.a1)
    return [a, b]
end

function cost_sincere(a::Spud)
    h_inc = [0,0,0,4,5,5,5,5,5,5]
    l_inc = [0,0,1,7,8,9,9,9,9,9]
    s_inc = [0,0,0,0,1,2,2,2,2,2]
    c = (h_inc[a.h] + s_inc[a.s]) * l_inc[a.l];
    if c < 6
        c = 6
    end
    return c
end

function fx_sincere(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f + 2, b.f + 2, b.p, b.r, b.s, b.a1)
    return [a, b]
end

function cost_cowardly(a::Spud)
    if (a.f < 3) || (a.h > 8)
        return 100
    end
    c = 0
    c2 = cost(a.h + 2, a.f - 2, a.l, a.p, a.r, a.s)
    c0 = cost(a.h, a.f, a.l, a.p, a.r, a.s)
    if c2 > c0
        c = c2 - c0
    end
    f_inc = [0,0,0,0,0,2,2,3,9,10]
    l_inc = [0,0,0,0,0,0,0,0,6,7]
    c += f_inc[a.f] + l_inc[a.l]
    if c < 6
        c = 6
    end
    return c
end

function fx_cowardly(a::Spud, b::Spud)
    if (a.p < b.p) || (a.r < b.r) || (a.s < b.s)
        a = Spud(a.name, a.h + 2, a.f - 2, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

ability_cost = Dict(
    ability_imitative => cost_imitative,
    ability_reciprocating => cost_reciprocating,
    ability_romantic => cost_romantic,
    ability_bibliophile => cost_bibliophile,
    ability_melodious => cost_melodious,
    ability_iconoclast => cost_iconoclast,
    ability_intoxicating => cost_intoxicating,
    ability_otaku => cost_otaku,
    ability_sincere => cost_sincere,
    ability_cowardly => cost_cowardly,
)

ability_fx = Dict(
    ability_imitative => fx_imitative,
    ability_reciprocating => fx_reciprocating,
    ability_romantic => fx_romantic,
    ability_bibliophile => fx_bibliophile,
    ability_melodious => fx_melodious,
    ability_iconoclast => fx_iconoclast,
    ability_intoxicating => fx_intoxicating,
    ability_otaku => fx_otaku,
    ability_sincere => fx_sincere,
    ability_cowardly => fx_cowardly,
)

const MXS = 100
const MXV = 10
const MNV = 1

function cost(h::Int64, f::Int64, l::Int64, p::Int64, r::Int64, s::Int64)::Int64
    #return h*(f+l-2*MNV) + f*(p+r+s-3*MNV) + h+f+l+p+r+s-6*MNV
    return h*(f+l) + f*(p+r+s)
end

function cost(a::Spud)::Int64
    if a.a1 == ability_none
        return cost(a.h, a.f, a.l, a.p, a.r, a.s)
    else
        return cost(a.h, a.f, a.l, a.p, a.r, a.s) + ability_cost[a.a1](a)
    end
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
    return [a.a1, a.h, a.f, a.l, a.p, a.r, a.s]
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





function apply_ability(a::Spud, b::Spud)::Array{Spud}
    if a.a1 == ability_none
        return [a, b]
    else
        return ability_fx[a.a1](a, b)
    end
end


function eval_battle(a::Spud, b::Spud)::Int64
    utb = compare_int_list(spud_utb_seq(a), spud_utb_seq(b), 0) # universal tiebreaker
    if utb == 0
        return 0
    end
    a_ability_first = compare_int_list([a.a1, a.s], [b.a1, b.s], utb)
    if a_ability_first ==1
        if a.a1 != ability_none
            res = apply_ability(a, b)
            a = res[1]
            b = res[2]
        end
        if b.a1 != ability_none
            res = apply_ability(b, a)
            b = res[1]
            a = res[2]
        end
    end
    if a_ability_first == -1
        if b.a1 != ability_none
            res = apply_ability(b, a)
            b = res[1]
            a = res[2]
        end
        if a.a1 != ability_none
            res = apply_ability(a, b)
            a = res[1]
            b = res[2]
        end
    end
    #println(a)
    #println(b)
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


function random_name_and_stat(ability::Int64 = ability_none)::Spud
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
    if ability == ability_none
        adj = adjectives[adj_i, :adjective]
        vp[1] = vp[1] + adjectives[adj_i, :H] * mult_adj
        vp[2] = vp[2] + adjectives[adj_i, :F] * mult_adj
        vp[3] = vp[3] + adjectives[adj_i, :L] * mult_adj
        vp[4] = vp[4] + adjectives[adj_i, :P] * mult_adj
        vp[5] = vp[5] + adjectives[adj_i, :R] * mult_adj
        vp[6] = vp[6] + adjectives[adj_i, :S] * mult_adj
    else
        adj = ability_name[ability]
    end
    job_i = rand(1:nrow(jobs))
    job = jobs[job_i, :job]
    vp[1] = vp[1] + jobs[job_i, :H] * mult_job
    vp[2] = vp[2] + jobs[job_i, :F] * mult_job
    vp[3] = vp[3] + jobs[job_i, :L] * mult_job
    vp[4] = vp[4] + jobs[job_i, :P] * mult_job
    vp[5] = vp[5] + jobs[job_i, :R] * mult_job
    vp[6] = vp[6] + jobs[job_i, :S] * mult_job
    name = string(adj, " ", job, " ", noun)
    Spud(name, vp[1], vp[2], vp[3], vp[4], vp[5], vp[6], ability_none)
end

function rand_rename(a::Spud, n_tries::Int = 100)::Spud
    best_score = 0.0
    best_b = random_name_and_stat(a.a1)
    for ii in 1:n_tries
        b = random_name_and_stat(a.a1)
        b_norm = sqrt(b.h^2 + b.f^2 + b.l^2 + b.p^2 + b.r^2 + b.s^2)
        score = (a.h * b.h + a.f * b.f + a.l * b.l + a.p * b.p + a.r * b.r + a.s * b.s)/b_norm
        if score > best_score
            best_score = score
            best_b = b
        end
    end
    return Spud(best_b.name, a.h, a.f, a.l, a.p, a.r, a.s, a.a1)
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

function spuds_to_df(as::Array{Spud})::DataFrame
    names = Array{String}(undef, length(as))
    hs = Array{Int}(undef, length(as))
    fs = Array{Int}(undef, length(as))
    ls = Array{Int}(undef, length(as))
    ps = Array{Int}(undef, length(as))
    rs = Array{Int}(undef, length(as))
    ss = Array{Int}(undef, length(as))
    a1s = Array{Int}(undef, length(as))
    for ii in 1:length(as)
        names[ii] = as[ii].name
        hs[ii] = as[ii].h
        fs[ii] = as[ii].f
        ls[ii] = as[ii].l
        ps[ii] = as[ii].p
        rs[ii] = as[ii].r
        ss[ii] = as[ii].s
        a1s[ii] = as[ii].a1
    end
    df = DataFrame(name = names, h = hs, f = fs, l = ls, p = ps, r = rs, s = ss, a1 = a1s)
    return df
end

function fpart(x::AbstractFloat)::AbstractFloat
  return x - trunc(x)
end

# for legacy dfs without abilities
function df_to_spuds0(df::DataFrame)::Array{Spud}
    n = size(df)[1]
    as = Array{Spud}(undef, n)
    for i in 1:n
        as[i] = Spud(df[i, :name], df[i, :h], df[i, :f], df[i, :l], df[i, :p], df[i, :r], df[i, :s], ability_none)
    end
    return as
end

function df_to_spuds(df::DataFrame)::Array{Spud}
    n = size(df)[1]
    as = Array{Spud}(undef, n)
    for i in 1:n
        as[i] = Spud(df[i, :name], df[i, :h], df[i, :f], df[i, :l], df[i, :p], df[i, :r], df[i, :s], df[i, :a1])
    end
    return as
end



function upgrade_spud(sp::Spud)::Spud
    h = sp.h
    f = sp.f
    l = sp.l
    p = sp.p
    r = sp.r
    s = sp.s
    a1 = sp.a1
    check_h = (h == MXV) || (cost(Spud("",h+1,f,l,p,r,s,a1)) > MXS)
    check_f = (f == MXV) || (cost(Spud("",h,f+1,l,p,r,s,a1)) > MXS)
    check_l = (l == MXV) || (cost(Spud("", h,f,l+1,p,r,s,a1)) > MXS)
    check_prs = (p+r+s == 3*MXV) || (cost(Spud("",h,f,l,p+1,r,s,a1)) > MXS)
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
        check_h = (h == MXV) || (cost(Spud("",h+1,f,l,p,r,s,a1)) > MXS)
        check_f = (f == MXV) || (cost(Spud("",h,f+1,l,p,r,s,a1)) > MXS)
        check_l = (l == MXV) || (cost(Spud("", h,f,l+1,p,r,s,a1)) > MXS)
        check_prs = (p+r+s == 3*MXV) || (cost(Spud("",h,f,l,p+1,r,s,a1)) > MXS)
    end
    return Spud(sp.name, h, f, l, p, r, s, a1)
end



function sample_library(ss_prob, n_init = 1000000)
    ffs = Array{Spud}(undef, n_init)
    ff_i = 0
    hrange = MNV:MXV
    frange = MNV:MXV
    lrange = MNV:MXV
    prange = MNV:MXV
    rrange = MNV:MXV
    srange = MNV:MXV
    for a1 in append!([k for k in keys(ability_name)], [ability_none])
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
                                                        ff = Spud("", h,f,l,p,r,s,a1)
                                                        if cost(ff) <= MXS
                                                            ff_i += 1
                                                            ffs[ff_i] = upgrade_spud(ff)
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
    end
    ffs = ffs[1:ff_i]
    return unique(ffs)
end

struct NashEquilibrium2p
    w1::Array{Float64}
    w2::Array{Float64}
    v::Float64
end


function payoffs2(set1::Array{Spud}, set2::Array{Spud})::Array{Int64}
    n1 = length(set1)
    n2 = length(set2)
    payoffs = Array{Int64}(undef, (n1, n2))
    for i in 1:n1
        for j in 1:n2
            payoffs[i,j] = eval_battle(set1[i], set2[j])
        end
    end
    return payoffs
end

function ffp2(set1::Array{Spud}, set2::Array{Spud}, nits::Int64)::NashEquilibrium2p
    n1 = length(set1)
    n2 = length(set2)
    payoffs = payoffs2(set1, set2)
    v1 = [1 for i in 1:n1]
    v2 = [1 for i in 1:n2]
    s1 = [sum(payoffs[i, :] .* v2) for i in 1:n1]
    s2 = [sum(payoffs[:, i] .* v1) for i in 1:n2]
    for it in 1:nits
        cands1 = findall(s1 .== maximum(s1))
        i1 = rand(cands1)
        cands2 = findall(s2 .== minimum(s2))
        i2 = rand(cands2)
        v1[i1] += 1
        s2 = s2 .+ payoffs[i1, :]
        v2[i2] += 1
        s1 = s1 .+ payoffs[:, i2]
    end
    w1 = v1./sum(v1)
    w2 = v2./sum(v2)
    val = sum([payoffs[i, j] * w1[i] * w2[j] for i in 1:n1 for j in 1:n2])
    return NashEquilibrium2p(w1, w2, val)
end

answ = []
for j in 1:100
    set1 = [rand(nash_env) for i in 1:3];
    set2 = [rand(nash_env) for i in 1:5];
    res = ffp2(set1, set2, 100)
    append!(answ, res.v)
end
println(mean(answ))
#answ

mutable struct BattleView
    round::Int64
    own::Array{Spud}
    owncount::Array{Int64}
    deployed::Array{Bool}
    obs::Array{Spud}
    obscount::Array{Int64}
end

mutable struct AiPlayer
    genteam::Function
    pick::Function
    create::Function
end

n_copy = 3

function initialize_game(team1::Array{Spud}, team2::Array{Spud})::Array{BattleView}
    teams = [team1, team2]
    bvs = Array{BattleView}(undef, 0)
    for team in teams
        bv = BattleView(0, team, [n_copy for i in 1:length(team)], [false for i in 1:length(team)], [], [])
        append!(bvs, [bv])
    end
    return bvs
end

function observe_fighters(game::Array{BattleView}, choices::Array{Int64})::Array{BattleView}
    for i in 1:2
        j = 3- i
        if choices[i] != 0
            game[i].deployed[choices[i]] = true
            ff = game[i].own[choices[i]]
            if ff in game[j].obs
                0
            else
                game[j].obs = append!(game[j].obs, [ff])
                game[j].obscount = append!(game[j].obscount, [game[i].owncount[choices[i]]])
            end
        end
    end
    return game
end

function modify_fighter_counts(game::Array{BattleView}, choices::Array{Int64}, mod::Array{Int64})::Array{BattleView}
    for i in 1:2
        j = 3- i
        if choices[i] != 0
            game[i].owncount[choices[i]] = game[i].owncount[choices[i]] + mod[i]
            ff = game[i].own[choices[i]]
            if ff in game[j].obs
                ind = findall(x -> x==ff, game[j].obs)[1]
                game[j].obscount[ind] = game[i].owncount[choices[i]]
            else
                0
            end
        end
    end
    return game
end

function play_combat(game::Array{BattleView}, choices::Array{Int64}, verbose::Bool = false)::Array{BattleView}
    round = game[1].round + 1
    fighters = [game[i].own[choices[i]] for i in 1:2]
    for i in 1:2
        @assert game[i].owncount[choices[i]] >= 0
    end
    if verbose
        print("Round ")
        print(round)
        print(": #A")
        print(choices[1])
        print(" ")
        print(fighters[1])
        print(" vs #B")
        print(choices[2])
        print(" ")
        println(fighters[2])
    end
    outcome = eval_battle(fighters[1], fighters[2])
    if verbose
        if outcome == 0
            println("Tied!")
        else
            if outcome == 1
                print(fighters[1])
            end
            if outcome == -1
                print(fighters[2])
            end
            println(" wins!")
        end
    end
    game = observe_fighters(game, choices)
    if outcome > -1
        game = modify_fighter_counts(game, choices, [0, -1])
    end
    if outcome < 1
        game = modify_fighter_counts(game, choices, [-1, 0])
    end
    for i in 1:2
        game[i].round = round
    end
    return game
end

function play_add_spud(game::Array{BattleView}, player::Int64, ff::Spud, verbose::Bool = false)::Array{BattleView}
    game[player].own = append!(game[player].own, [ff])
    game[player].owncount = append!(game[player].owncount, [n_copy])
    game[player].deployed = append!(game[player].deployed, [false])
    if verbose
        print("Player ")
        print(player)
        print(" adds #")
        print(["A", "B"][player])
        print(length(game[player].own))
        print(" ")
        println(ff)
    end
    return game
end

function play_is_game_over(game::Array{BattleView})::Bool
    for bv in game
        if sum(bv.owncount) == 0
            return true
        end
    end
    return false
end

nash_env_df = DataFrame(CSV.File("spudsD_mxv10_nash.csv"))
nash_env = df_to_spuds0(nash_env_df)
counts = [parse(Int, s.name[2:end]) for s in nash_env]
nash_env = [rand_rename(ff) for ff in nash_env]
w = counts./sum(counts);

function generate_rand_team(env, counts, nteam)
    cc = cumsum(counts)
    team = Array{Spud}(undef, nteam)
    for i in 1:nteam
        tmp = rand(1:sum(counts))
        ind = sum(cc .< tmp)+1
        team[i] = env[ind]
    end
    return team
end

function ai0_pick(a::BattleView)::Int64
    inds = findall(a.owncount .> 0)
    return rand(inds)
end

function ai0_create(a::BattleView)::Spud
    return generate_rand_team(nash_env, counts, 1)[1]
end

function ai1_create(a::BattleView)::Spud
    ops = a.obs[a.obscount .> 0]
    ebs = [eval_battle_list(ff, ops) for ff in nash_env]
    cands = nash_env[ebs .== maximum(ebs)]
    return rand(cands)
end

lib = sample_library(0.2)

function ai2_create(a::BattleView)::Spud
    ops = a.obs[a.obscount .> 0]
    ebs = [eval_battle_list(ff, ops) for ff in nash_env]
    if maximum(ebs) == length(ops)
        cands = nash_env[ebs .== maximum(ebs)]
        return rand(cands)
    else
        ebs2 = [eval_battle_list(ff, ops) for ff in lib]
        if maximum(ebs2) > maximum(ebs)
            cands = lib[ebs2 .== maximum(ebs2)]
            return rand_rename(rand(cands))
        else
            cands = nash_env[ebs .== maximum(ebs)]
            return rand(cands)
        end
    end
end


rand()

# picks Spud most likely to win
function ai_greedy_pick(a::BattleView)::Int64
    if length(a.obs[a.obscount .> 0]) == 0
        return rand(findall(a.owncount .> 0))
    end
    res = ffp2(a.own[a.owncount .> 0], a.obs[a.obscount .> 0], 100)
    ind = findall(a.owncount .> 0)[rand(findall(res.w1 .== maximum(res.w1)))]
    return ind
end

# picks from spuds already deployed
function ai_conservative_pick(a::BattleView)::Int64
    own = a.own[a.owncount .> 0 .&& a.deployed]
    owninds = findall(a.owncount .> 0 .&& a.deployed)
    if length(own) == 0
        # pick greedy
        return ai_greedy_pick(a)
    end
    ops = a.obs[a.obscount .> 0]
    res = ffp2(own, ops, 100)
    ind = owninds[rand(findall(res.w1 .== maximum(res.w1)))]
    return ind
end

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_conservative_pick, ai1_create)
ais = [ai1, ai2]
play_game(ais, true)

# picks Spud that counters opp
function ai_antigreedy_pick(a::BattleView)::Int64
    if length(a.obs[a.obscount .> 0]) == 0
        return rand(findall(a.owncount .> 0))
    end
    own = a.own[a.owncount .> 0]
    ops = a.obs[a.obscount .> 0]
    res = ffp2(own, ops, 100)
    ind2 = rand(findall(res.w2 .== maximum(res.w2)))
    ebs = [eval_battle(ff, ops[ind2]) for ff in own]
    if sum(ebs .== 1) > 1
        inds = findall(ebs .== 1)
        ind = inds[rand(findall(res.w1[inds].==maximum(res.w1[inds])))]
    else
        ind = rand(findall(ebs .== maximum(ebs)))
    end
    return findall(a.owncount .> 0)[ind]
end

# picks Spud based on 1-round nash
function ai_myopic_pick(a::BattleView)::Int64
    if length(a.obs[a.obscount .> 0]) == 0
        return rand(findall(a.owncount .> 0))
    end
    res = ffp2(a.own[a.owncount .> 0], a.obs[a.obscount .> 0], 100)
    cc = cumsum(res.w1)
    ind = sum(cc .< rand()) + 1
    return findall(a.owncount .> 0)[ind]
end

creation_rounds = [5, 10, 15]

function play_game(players::Array{AiPlayer}, verbose::Bool = true)::Int64
    ai1 = players[1]
    ai2 = players[2]
    ais = players
    team1 = ai1.genteam()
    team2 = ai2.genteam()
    game = initialize_game(team1, team2)
    while !play_is_game_over(game)
        # play a round
        choice1 = ai1.pick(game[1])
        choice2 = ai2.pick(game[2])
        game = play_combat(game, [choice1, choice2], verbose)
        if verbose
            for i in 1:2
                println(game[i].owncount)
            end
        end
        if game[1].round in creation_rounds
            for i in 1:2
                ff = ais[i].create(game[i])
                game = play_add_spud(game, i, ff, verbose)
            end
        end
    end
    if sum(game[1].owncount) == 0
        if sum(game[2].owncount) == 0
            return 0
        else
            return -1
        end
    end
    return 1
end

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ais = [ai1, ai2]
play_game(ais, true)

players = ais
verbose = true

ai1 = players[1]
ai2 = players[2]
ais = players
team1 = ai1.genteam()
team2 = ai2.genteam()
game = initialize_game(team1, team2)
while !play_is_game_over(game)
    # play a round
    choice1 = ai1.pick(game[1])
    choice2 = ai2.pick(game[2])
    game = play_combat(game, [choice1, choice2], verbose)
    if verbose
        for i in 1:2
            println(game[i].owncount)
        end
    end
    if game[1].round in creation_rounds
        for i in 1:2
            ff = ais[i].create(game[i])
            game = play_add_spud(game, i, ff, verbose)
        end
    end
end

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai0_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai0_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:1000]
mean(res .> 0)


ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai0_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:10000];
mean(res .> 0)
# explore effect of creation rounds on advantage of non-naive play
# 1. comparing naive creation to smart create
creation_rounds = [5]         # 0.6430
creation_rounds = [10]        # 0.7075
creation_rounds = [13]        # 0.7171
creation_rounds = [14]        # 0.7245
creation_rounds = [15]        # 0.7281
creation_rounds = [16]        # 0.7170
creation_rounds = [17]        # 0.7205
creation_rounds = [20]        # 0.7097
creation_rounds = [30]        # 0.5003
creation_rounds = [5, 15]     # 0.7423
creation_rounds = [5, 20]     # 0.7683
creation_rounds = [5, 25]     # 0.7674
creation_rounds = [7, 20]     # 0.7721
creation_rounds = [7, 23]     # 0.7738
creation_rounds = [8, 23]     # 0.7752
creation_rounds = [8, 24]     # 0.7796
creation_rounds = [8, 25]     # 0.7844
creation_rounds = [8, 27]     # 0.7648
creation_rounds = [9, 25]     # 0.7852
creation_rounds = [10, 25]    # 0.7740
creation_rounds = [15, 20]    # 0.7383


ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:10000];
mean(res .> 0)

# 1. given smart create, compare naive pick to greedy pick
creation_rounds = [5]         # 0.
creation_rounds = [10]        # 0.
creation_rounds = [13]        # 0.
creation_rounds = [14]        # 0.
creation_rounds = [15]        # 0.6464
creation_rounds = [16]        # 0.
creation_rounds = [17]        # 0.6994
creation_rounds = [20]        # 0.
creation_rounds = [30]        # 0.
creation_rounds = [5, 15]     # 0.
creation_rounds = [5, 20]     # 0.
creation_rounds = [5, 25]     # 0.
creation_rounds = [7, 20]     # 0.
creation_rounds = [7, 23]     # 0.
creation_rounds = [8, 23]     # 0.
creation_rounds = [8, 24]     # 0.
creation_rounds = [8, 25]     # 0.
creation_rounds = [8, 27]     # 0.
creation_rounds = [9, 25]     # 0.8287
creation_rounds = [10, 25]    # 0.
creation_rounds = [15, 20]    # 0.



ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100]
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai2_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100]
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100];
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_myopic_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100];
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_myopic_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100];
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
ais = [ai1, ai2]
@time res = [play_game(ais, false) for i in 1:100];
mean(res .> 0)

ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai2_create)
ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai0_pick, ai1_create)
ais = [ai1, ai2]

@time lib = sample_library(0.1);
