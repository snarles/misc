using Random
using DataFrames
using CSV
using DelimitedFiles
using Statistics
using Printf
using LinearAlgebra
using Dates
using MAT

struct Spud
    name::String
    h::Int64
    f::Int64
    l::Int64
    p::Int64
    r::Int64
    s::Int64
    a1::Int64
    cost::Int64
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

function fx_imitative(a::Spud, b::Spud)
    if b.h > a.h
        a = Spud(a.name, b.h, a.f, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function fx_reciprocating(a::Spud, b::Spud)
    if b.f > a.f
        a = Spud(a.name, a.h, b.f, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function fx_romantic(a::Spud, b::Spud)
    if b.p > a.l
        a = Spud(a.name, a.h, a.f, b.p, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function fx_bibliophile(a::Spud, b::Spud)
    if b.r > a.l
        a = Spud(a.name, a.h, a.f, b.r, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

function fx_melodious(a::Spud, b::Spud)
    if b.p > b.r && b.p > b.s
        a = Spud(a.name, a.h, a.f, a.r, a.p + 3, a.r, a.s, a.a1)
    end
    return [a, b]
end

function fx_iconoclast(a::Spud, b::Spud)
    v = sum([b.h,b.f,b.l,b.p,b.r,b.s] .== MXV)
    if v >= 3
        aa = [a.h, a.f, a.l, a.p, a.r, a.s]
        aa2 = map(x -> div(x, 4), aa)
        r = aa .+ aa2
        a = Spud(a.name, r[1], r[2], r[3], r[4], r[5], r[6], a.a1)
    end
    return [a, b]
end

function fx_intoxicating(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f, b.l, b.p, b.r, b.f, b.a1)
    return [a, b]
end

function fx_otaku(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f, b.l, b.p, b.h, b.s, b.a1)
    return [a, b]
end


function fx_sincere(a::Spud, b::Spud)
    b = Spud(b.name, b.h, b.f + 2, b.f + 2, b.p, b.r, b.s, b.a1)
    return [a, b]
end

function fx_cowardly(a::Spud, b::Spud)
    if (a.p < b.p) || (a.r < b.r) || (a.s < b.s)
        a = Spud(a.name, a.h + 2, a.f - 2, a.l, a.p, a.r, a.s, a.a1)
    end
    return [a, b]
end

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
const MXV = 9
const MNV = 1

# function cost(h::Int64, f::Int64, l::Int64, p::Int64, r::Int64, s::Int64)::Int64
#     #return h*(f+l-2*MNV) + f*(p+r+s-3*MNV) + h+f+l+p+r+s-6*MNV
#     return h*(f+l) + f*(p+r+s)
# end

# function cost(a::Spud)::Int64
#     if a.a1 == ability_none
#         return cost(a.h, a.f, a.l, a.p, a.r, a.s)
#     else
#         return cost(a.h, a.f, a.l, a.p, a.r, a.s) + ability_cost[a.a1](a)
#     end
# end

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
    cs = Array{Int}(undef, length(as))
    for ii in 1:length(as)
        names[ii] = as[ii].name
        hs[ii] = as[ii].h
        fs[ii] = as[ii].f
        ls[ii] = as[ii].l
        ps[ii] = as[ii].p
        rs[ii] = as[ii].r
        ss[ii] = as[ii].s
        a1s[ii] = as[ii].a1
        cs[ii] = as[ii].cost
    end
    df = DataFrame(name = names, h = hs, f = fs, l = ls, p = ps, r = rs, s = ss, a1 = a1s, cost = cs)
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
        as[i] = Spud(df[i, :name], df[i, :h], df[i, :f], df[i, :l], df[i, :p], df[i, :r], df[i, :s], ability_none, -1)
    end
    return as
end

function df_to_spuds(df::DataFrame)::Array{Spud}
    n = size(df)[1]
    as = Array{Spud}(undef, n)
    for i in 1:n
        as[i] = Spud(df[i, :name], df[i, :h], df[i, :f], df[i, :l], df[i, :p], df[i, :r], df[i, :s], df[i, :a1], df[i, :cost])
    end
    return as
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
    for i in 1:n_nash
        counts[i] += 1
        for j in 1:n_lose[i]
            i_w = i_lose[i, j]
            wins[i_w]+= 1
        end
    end
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

function jobify_fighter_counts(game::Array{BattleView}, choices::Array{Int64}, job::Array{Int64})::Array{BattleView}
    for i in 1:2
        j = 3- i
        if choices[i] != 0
            game[i].owncount[choices[i]] = game[i].owncount[choices[i]] + job[i]
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
        game = jobify_fighter_counts(game, choices, [0, -1])
    end
    if outcome < 1
        game = jobify_fighter_counts(game, choices, [-1, 0])
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

# ai1 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
# ai2 = AiPlayer(() -> generate_rand_team(nash_env, counts, 5), ai_greedy_pick, ai1_create)
# ais = [ai1, ai2]
# play_game(ais, true)

#@time lib = sample_library(1.01, [999]);

function filter_nondominated(as::Array{Spud})::Array{Spud}
    df = spuds_to_df(as)
    mat = Array{Int64}(undef, (length(as), 6))
    mat[:, 1] = df.h
    mat[:, 2] = df.f
    mat[:, 3] = df.l
    mat[:, 4] = df.p
    mat[:, 5] = df.r
    mat[:, 6] = df.s;
    isDominated = zeros(Int64, length(as));
    for i in 1:length(as)
        v = mat[i, :]
        bv = ones(Int64, length(as))
        for j in 1:6
            bv = bv .* (mat[:, j] .>= v[j])
        end
        if sum(bv) > 1
            isDominated[i] = 1
        end
    end
    return as[isDominated .== 0]
end

function spud_to_vec(a::Spud)::Array{Int64}
    return [a.h, a.f, a.l, a.p, a.r, a.s]
end

struct SpudBase
    name::String
    h::Int64
    f::Int64
    l::Int64
    p::Int64
    r::Int64
    s::Int64
    a1::Int64
    cost::Int64
end

struct SpudMod
    name::String
    h::Int64
    f::Int64
    l::Int64
    p::Int64
    r::Int64
    s::Int64
    a1::Int64
    minh::Int64
    minf::Int64
    minl::Int64
    minp::Int64
    minr::Int64
    mins::Int64
    cost::Int64
end

function rename_base(a::SpudBase, s::String)::SpudBase
    return SpudBase(s, a.h,a.f,a.l,a.p,a.r,a.s,a.a1,a.cost)
end

function rename_job(a::SpudMod, s::String)::SpudMod
    return SpudMod(s, a.h,a.f,a.l,a.p,a.r,a.s,a.a1,a.minh,a.minf,a.minl,a.minp,a.minr,a.mins,a.cost)
end

function combine_base_mod(b::SpudBase, j::SpudMod)::Spud
    v = [b.h,b.f,b.l,b.p,b.r,b.s]
    c = b.cost
    a1 = b.a1
    if a1 == 999
        a1 = j.a1
    end
    w = [j.h,j.f,j.l,j.p,j.r,j.s]
    m = [j.minh,j.minf,j.minl,j.minp,j.minr,j.mins]
    c2 = j.cost
    if sum(v .< m) == 0
        c2 -= discount
    end
    z = v .+ w
    z[z .> MXV] .= MXV
    return Spud(string(j.name, " ", b.name),z[1],z[2],z[3],z[4],z[5],z[6],a1,c+c2)
end

function combine_spud_job(b::Spud, j::SpudMod)::Spud
    v = [b.h,b.f,b.l,b.p,b.r,b.s]
    c = b.cost
    a1 = b.a1
    if a1 == 999
        a1 = j.a1
    end
    w = [j.h,j.f,j.l,j.p,j.r,j.s]
    m = [j.minh,j.minf,j.minl,j.minp,j.minr,j.mins]
    c2 = j.cost
    if sum(v .< m) == 0
        c2 -= discount
    end
    z = v .+ w
    z[z .> MXV] .= MXV
    return Spud(string(j.name, " ", b.name),z[1],z[2],z[3],z[4],z[5],z[6],a1,c+c2)
end

function check_validity(a::Spud)::Bool
    return minimum([a.h,a.f,a.l,a.p,a.r,a.s]) >= MNV
end

function get_bases(scores, min_cost_base, max_cost_base, n_base_parts)::Array{SpudBase}
    bases = Array{SpudBase}(undef, n_base_parts)
    for i_base in 1:n_base_parts
        desired_cost = rand(min_cost_base:max_cost_base)
        indx = rand(findall(scores .== desired_cost))
        bases[i_base] = SpudBase("base",indx[1],indx[2],indx[3],indx[4],indx[5],indx[6],999,desired_cost)
    end
    bases = unique(bases)
    n_base_parts1 = length(bases)
    bases = bases[sortperm([x.cost for x in bases])]
    bases = [rename_base(bases[i], string("b", i)) for i in 1:n_base_parts1]
    return bases
end

function get_mods(scores, discount, bases, min_mod_delt, max_mod_delt, min_cost_mod, max_cost_mod, n_mods)::Array{SpudMod}
    n_base_parts1 = length(bases)
    # generate mods
    mods = Array{SpudMod}(undef, n_mods)
    ff_i = 0
    flag = true
    while flag
        v = [0,0,0,0,0,0]
        m = [0,0,0,0,0,0]
        for j in 1:3
            v[rand(1:6)] = rand(min_mod_delt:max_mod_delt)
            if rand() < 0.3
                m[rand(1:6)] = rand(1:9)
            end
        end
        jj = SpudMod("mod",v[1],v[2],v[3],v[4],v[5],v[6],999,m[1],m[2],m[3],m[4],m[5],m[6],0)
        max_diff = 0
        for i_base in 1:n_base_parts1
            b = bases[i_base]
            bv = [b.h,b.f,b.l,b.p,b.r,b.s]
            b2 = bv .+ v
            b2[b2 .> MXV] .= MXV
            if minimum(b2) >= MNV
                c = b.cost
                # min cost discount
                if sum(bv .< m) == 0
                    c -= discount
                end
                cost2 = scores[CartesianIndex((b2...))]
                diff = cost2 - c
                if diff > max_diff
                    max_diff = diff
                end
            end
        end
        if max_diff >= min_cost_mod && max_diff <= max_cost_mod
            ff_i += 1
            mods[ff_i] = SpudMod("mod",v[1],v[2],v[3],v[4],v[5],v[6],999,m[1],m[2],m[3],m[4],m[5],m[6],max_diff)
        end
        if ff_i >= n_mods
            flag = false
        end
    end
    mods = unique(mods);
    mods = mods[sortperm([j.cost for j in mods])]
    n_mods1 = length(mods)
    mods = [rename_job(mods[i], string("m",i)) for i in 1:n_mods1]
    return mods
end



function get_base_mod_combs(bases, mods)::Array{Spud}
    # get all Spuds from base + mod combo
    lib2 = Array{Spud}(undef, length(bases)*length(mods))
    ff_i = 0
    for b in bases
        for j in mods
            ff = combine_base_mod(b,j)
            if ff.cost <= MXS && check_validity(ff)
                ff_i += 1
                lib2[ff_i] = ff
            end
        end
    end
    lib2 = lib2[1:ff_i]
    return lib2
end


function get_jobs(scores, discount, lib2, min_job_delt, max_job_delt, min_cost_job, max_cost_job, n_jobs)::Array{SpudMod}
    # generate jobs
    jobs = Array{SpudMod}(undef, n_jobs)
    ff_i = 0
    flag = true

    while flag
        v = [0,0,0,0,0,0]
        m = [0,0,0,0,0,0]
        for j in 1:3
            v[rand(1:6)] = rand(min_job_delt:max_job_delt)
            if rand() < 0.3
                m[rand(1:6)] = rand(1:9)
            end
        end
        jj = SpudMod("job",v[1],v[2],v[3],v[4],v[5],v[6],999,m[1],m[2],m[3],m[4],m[5],m[6],0)
        max_diff = 0
        for b in lib2
            bv = [b.h,b.f,b.l,b.p,b.r,b.s]
            b2 = bv .+ v
            b2[b2 .> MXV] .= MXV
            if minimum(b2) >= MNV
                c = b.cost
                # min cost discount
                if sum(bv .< m) == 0
                    c -= discount
                end
                cost2 = scores[CartesianIndex((b2...))]
                diff = cost2 - c
                if diff > max_diff
                    max_diff = diff
                end
            end
        end
        if max_diff >= min_cost_job && max_diff <= max_cost_job
            ff_i += 1
            jobs[ff_i] = SpudMod("job",v[1],v[2],v[3],v[4],v[5],v[6],999,m[1],m[2],m[3],m[4],m[5],m[6],max_diff)
        end
        if ff_i >= n_jobs
            flag = false
        end
    end

    jobs = unique(jobs);
    jobs = jobs[sortperm([j.cost for j in jobs])]
    n_jobs1 = length(jobs)
    jobs = [rename_job(jobs[i], string("j",i)) for i in 1:n_jobs1]
    return jobs
end

function get_library(discount, bases, mods, jobs, filter::Bool= true)::Array{Spud}
    # Generate Spuds

    library = Array{Spud}(undef, length(bases)*length(mods)*length(jobs))
    ff_i = 0

    for b in bases
        for j in mods
            s1 = combine_base_mod(b, j)
            if check_validity(s1) && s1.cost <= MXS
                for m in jobs
                    s2 = combine_spud_job(s1, m)
                    if check_validity(s2) && s2.cost <= MXS
                        ff_i += 1
                        library[ff_i] = s2
                    end
                end
            end
        end
    end
    library = library[1:ff_i]
    if filter
      library = filter_nondominated(library)
    end
    return library
end

function generate_parts(n_base_parts, n_mods, n_jobs,
        min_cost_base, max_cost_base, min_cost_mod, max_cost_mod,
        min_mod_delt, max_mod_delt, min_job_delt, max_job_delt, discount)
    bases = get_bases(scores, min_cost_base, max_cost_base, n_base_parts)
    n_base_parts1 = length(bases)
    mods = get_mods(scores, discount, bases, min_mod_delt, max_mod_delt, min_cost_mod, max_cost_mod, n_mods)
    n_mods1 = length(mods)
    lib2 = get_base_mod_combs(bases, mods)
    jobs = get_jobs(scores, discount, lib2, min_job_delt, max_job_delt, min_cost_job, max_cost_job, n_jobs)
    library = get_library(discount, bases, mods, jobs)
    counts = ffp(library, 100000);
    diversity = sum(counts .> 10)
    return Dict("bases" => bases, "mods" => mods, "jobs" => jobs, "diversity" => diversity)
end

scores = matread("scores_sq300.mat")["scores"];

n_base_parts = 100
n_mods = 100
n_jobs = 100
min_cost_base = 15
max_cost_base = 25
min_cost_mod = 25
max_cost_mod = 50
min_cost_job = 35
max_cost_job = 70

min_mod_delt = -2
max_mod_delt = 8

min_job_delt = 0
max_job_delt = 3

discount = 20

n_its = 100
divs = Array{Int64}(undef, n_its);

@time res = generate_parts(n_base_parts, n_mods, n_jobs,
        min_cost_base, max_cost_base, min_cost_mod, max_cost_mod,
        min_mod_delt, max_mod_delt, min_job_delt, max_job_delt, discount);

lib = get_library(discount, res["bases"], res["mods"], res["jobs"],false);
nms = [ff.name for ff in lib];
bjs = [string(split(nm, " ")[3], split(nm, " ")[1]) for nm in nms];
length(unique(bjs))
length(res["jobs"]) * length(res["bases"])

best_res = res
best_div = res["diversity"]
for i in 1:n_its
    res = generate_parts(n_base_parts, n_mods, n_jobs,
            min_cost_base, max_cost_base, min_cost_mod, max_cost_mod,
            min_mod_delt, max_mod_delt, min_job_delt, max_job_delt, discount);
    divs[i] = res["diversity"]
    if divs[i] > best_div
        best_div = divs[i]
        best_res = res
        print(length(res["mods"]))
        print(" ")
        print(length(res["jobs"]))
        print(" ")
        println(best_div)
    end
end
best_div
mean(divs)

##   n_b  n_j  n_m   mu(div) max-div time_per
