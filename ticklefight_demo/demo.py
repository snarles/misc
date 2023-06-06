# Hide and Seek Tickle Fight, by Charles Zheng and Helen Xu 2023
# CC0 license
import pandas as pd
import numpy as np
from numpy.random import choice, rand

print('Welcome to Hide-and-Find Tickle Fight!')
print('Here is a demo to teach you the rules and allow you to play against the computer.')
print('If you have never played before, start by playing vs CPU.')

mainflag = True
fighters = None
# data needed to generate names
subs = np.loadtxt('name_parts.txt', dtype = str)
initial_subs = subs[:396]
heads = {}
for s in subs:
    head = s[0:2]
    tail = s[1:3]
    heads[head] = []
    heads[tail] = []
for s in subs:
    head = s[0:2]
    heads[head].append(s)
adjectives = pd.read_csv('adjectives.csv')
nouns = pd.read_csv('nouns.csv')

adjectives_inds = {}
nouns_inds = {}

letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

for l in letters:
    inds_adj = [i for i in range(len(adjectives)) if adjectives.adjective.values[i][0] == l]
    adjectives_inds[l] = inds_adj
    inds_nouns = [i for i in range(len(nouns)) if nouns.noun.values[i][0] == l]
    nouns_inds[l] = inds_nouns


def print_stat_comparison(f1, f2, v_i, w_i, comparisons):
    v = f1[1:]
    w = f2[1:]
    nm1 = f1[0]
    nm2 = f2[0]
    statnames = ['HIDE', 'FIND', 'LUCK', 'TICKLE']
    if '>' in comparisons and v[v_i] > w[w_i]:
        rel = 'greater than'
    if '>=' in comparisons and v[v_i] >= w[w_i]:
        #rel = 'greater-than-or-equal-to'
        rel = '>='
    if '==' in comparisons and v[v_i] == w[w_i]:
        rel = 'equal to'
    if '<=' in comparisons and v[v_i] <= w[w_i]:
        #rel = 'less-than-or-equal-to'
        rel = '<='
    if '<' in comparisons and v[v_i] < w[w_i]:
        rel = 'less than'
    print('%s\'s %s %i is %s %s\'s %s %i.' % (nm1, statnames[v_i], v[v_i], rel, nm2, statnames[w_i], w[w_i]))

def random_name(name = None, req_len = None):
    if name is None:
        name = initial_subs[choice(len(initial_subs))]
    flag = True
    while flag:
        s = name[-2:]
        ws = heads[s]
        if len(ws) == 0:
            flag = False
        else:
            if req_len is None:
                w = ws[choice(len(ws))]
                name = name + w[-1]
            elif len(name) >= req_len:
                sub_ws = [w for w in ws if w[-1] == '.']
                if len(sub_ws) > 0:
                    w = sub_ws[choice(len(sub_ws))]
                else:
                    w = ws[choice(len(ws))]
                name = name + w[-1]
            elif len(name) < req_len:
                sub_ws = [w for w in ws if w[-1] != '.']
                if len(sub_ws) > 0:
                    w = sub_ws[choice(len(sub_ws))]
                else:
                    w = ws[choice(len(ws))]
                name = name + w[-1]
    return name[:-1]

def random_name_and_stat():
    type = choice(5, p=[0.2,0.2,0.2,0.2,0.2])
    allit = choice(2, p = [0.8, 0.2])
    v = np.array([0,0,0,0])
    # name + adjective/noun
    if type in [0,1,2,3]:
        len_nm = choice(5) + 3
        nm = random_name(req_len = len_nm)
        if type in [0,1]:
            inds = range(len(adjectives))
            if allit==1:
                let = nm[0]
                inds = adjectives_inds[let]
            ind_a = inds[choice(len(inds))]
            adj = adjectives.adjective.values[ind_a]
            v = v + np.array(adjectives.iloc[ind_a, 1:5])
            if type == 0:
                name = nm + ' the ' + adj
            if type == 1:
                name = adj + ' ' + nm
        if type in [2,3]:
            inds = range(len(nouns))
            if allit==1:
                let = nm[0]
                inds = nouns_inds[let]
            ind_n = inds[choice(len(inds))]
            noun = nouns.noun.values[ind_n]
            v = v + np.array(nouns.iloc[ind_n, 1:5])
            if type == 2:
                name = nm + ' the ' + noun
            if type == 3:
                name = noun + ' ' + nm
    # adjective noun
    if type==4:
        ind_n = choice(len(nouns))
        noun = nouns.noun.values[ind_n]
        v = np.array(nouns.iloc[ind_n, 1:5])
        inds = range(len(adjectives_inds))
        if allit==1:
            let = noun[0]
            inds = adjectives_inds[let]
        ind_a = inds[choice(len(inds))]
        adj = adjectives.adjective.values[ind_a]
        v = v + np.array(adjectives.iloc[ind_a, 1:5])
        name = adj + ' ' + noun
    return name, v




def eval_battle(f1, f2, verbose = True):
    v = f1[1:]
    w = f2[1:]
    nm1 = f1[0]
    nm2 = f2[0]
    outcome = 'unknown'
    if verbose:
        print('\nIt\'s a tickle duel!\n')
        ff = list_to_fighter_df([f1,f2])
        ff.index = ['You', 'CPU']
        print(ff)
        print('\nWho will win?\nLet\'s go through the battle slowly...\n')
        input('[Press Enter]: ')
        print('First, check which fighters have F greater-than-or-equal-to the opponent\'s H.')
        input('[Press Enter]: ')
        print_stat_comparison(f1, f2, 1, 0, ['>=', '<'])
        print_stat_comparison(f2, f1, 1, 0, ['>=', '<'])
        input('[Press Enter]: ')
    f1_finds = (v[1] >= w[0])
    f2_finds = (w[1] >= v[0])
    # mutual H <= F -> check T
    if f1_finds and f2_finds:
        if verbose:
            print('Both fighters can find each other.  Therefore, a full-frontal tickle battle begins!')
            print('Now check which fighter has the greatest T, or if they are tied...')
            input('[Press Enter]: ')
            print_stat_comparison(f1, f2, 3, 3, ['<', '==', '>'])
            input('[Press Enter]: ')
        # mutual H<=F, T tied -> check L
        if v[3] == w[3]:
            if verbose:
                print("Therefore, both fighters appeared evenly matched in the tickle fight.")
                print("Now, check which fighter has the greatest L, or if they are tied...")
                input('\n[Press Enter]: ')
                print_stat_comparison(f1,f2,2,2,['<','==','>'])
                input('\n[Press Enter]: ')
            # mutual H<=F, T tied, L tied -> check H
            if v[2]==w[2]:
                if verbose:
                    print("So, while both fighters appeared evenly matched in the tickle fight, \nboth got a lucky break and found each other's weakness!")
                    print("Now, check which fighter has the greatest H, or if they are tied...")
                    input('\n[Press Enter]: ')
                    print_stat_comparison(f1,f2,0,0,['<','==','>'])
                    input('\n[Press Enter]: ')
                # mutual H<=F, T tied, L tied, H tied -> draw
                if v[0]==w[0]:
                    outcome = 'draw'
                    if verbose:
                        print("So, both fighters were able to block the other from hitting their weakness!  It's a tie and both fighters lose!")
                        input('\n[Press Enter]: ')
                # mutual H<=F, T tied, L tied, H not tied -> win/lose
                else:
                    if v[0] > w[0]:
                        outcome = 'win'
                        w_nm = nm1
                        l_nm = nm2
                    else:
                        outcome = 'lose'
                        w_nm = nm2
                        l_nm = nm1
                    if verbose:
                        print("So, while %s tried to tickle %s\'s weak spot, %s was able to block. \nThis gave %s the chance to tickle %s\'s weak spot first.  %s won!" % (l_nm, w_nm, w_nm, w_nm, l_nm, w_nm))
                        input('[Press Enter]: ')
            # mutual H<=F, T tied, L not tied -> win/lose
            else:
                if v[2] > w[2]:
                    outcome = 'win'
                    w_nm = nm1
                    l_nm = nm2
                else:
                    outcome = 'lose'
                    w_nm = nm2
                    l_nm = nm1
                if verbose:
                    print("So even though both fighters were evenly matched in the tickle fight at first, \n%s was able to find %s\'s weakness and win!" % (w_nm, l_nm))
                    input('[Press Enter]: ')
        # mutual H<=F, T not tied -> win/lose
        else:
            if v[3] > w[3]:
                outcome = 'win'
                w_nm = nm1
                l_nm = nm2
            else:
                outcome = 'lose'
                w_nm = nm2
                l_nm = nm1
            if verbose:
                print("Therefore, %s won the tickle fight!" % w_nm)
                input('\n[Press Enter]: ')
    # exclusive H <= F -> win/lose
    elif (f1_finds and not f2_finds) or (f2_finds and not f1_finds):
        if f1_finds and not f2_finds:
            outcome = 'win'
            w_nm = nm1
            l_nm = nm2
        else:
            outcome = 'lose'
            w_nm = nm2
            l_nm = nm1
        if verbose:
            print('%s is able to find %s without getting spotted!' % (w_nm, l_nm))
            print("Therefore, %s was able to sneak up on %s and tickle %s first!  %s won!" % (w_nm, l_nm, l_nm, w_nm))
            input('\n[Press Enter]: ')
    # mutual H > F -> check L
    else:
        if verbose:
            print('Neither fighter can spot the other.  It\'s a race to see who will luck out and find their opponent first!')
            print('Now, check which fighter has the greatest L, or if they are tied...')
            input('\n[Press Enter]: ')
            print_stat_comparison(f1, f2, 2, 2, ['<', '==', '>'])
            input('\n[Press Enter]: ')
        # mutual H > F, L tied -> check T
        if v[2] == w[2]:
            if verbose:
                print("So even though both fighters spent time looking for each other, \nas luck would have it both fighters spotted each other by chance at the same time and got into a tickle fight!")
                print('Now check which fighter has the greatest T, or if they are tied...')
                input('\n[Press Enter]: ')
                print_stat_comparison(f1,f2,3,3,['<','==','>'])
                input('\n[Press Enter]: ')
            # mutual H > F, L tied, T tied -> check F
            if v[3]==w[3]:
                if verbose:
                    print("So, while both fighters appeared evenly matched in the tickle fight, \nboth got a lucky break and found each other's weakness!")
                    print("Now, check which fighter has the greatest F, or if they are tied...")
                    input('\n[Press Enter]: ')
                    print_stat_comparison(f1,f2,1,1,['<','==','>'])
                    input('\n[Press Enter]: ')
                # mutual H > F, L tied, T tied, F tied -> draw
                if v[1]==w[1]:
                    outcome = 'draw'
                    if verbose:
                        print("So, both fighters were able to find the other's weakness!  They hit at the same time, and both get taken out.  It's a tie and both fighters lose!")
                        input('\n[Press Enter]: ')
                # mutual H > F, L tied, T tied, F not tied -> win/lose
                else:
                    if v[1] > w[1]:
                        outcome = 'win'
                        w_nm = nm1
                        l_nm = nm2
                    else:
                        outcome = 'lose'
                        w_nm = nm2
                        l_nm = nm1
                    if verbose:
                        print("So even though both fighters were evenly matched in the tickle fight at first, \n%s was able to find %s\'s weakness and win!  %s won!" % (w_nm, l_nm))
                        input('[Press Enter]: ')
            # mutual H > F, L tied, T not tied -> win/lose
            else:
                if v[3] > w[3]:
                    outcome = 'win'
                    w_nm = nm1
                    l_nm = nm2
                else:
                    outcome = 'lose'
                    w_nm = nm2
                    l_nm = nm1
                if verbose:
                    print("Therefore, %s won the tickle fight!" % w_nm)
                    input('\n[Press Enter]: ')
        # mutual H > F, L not tied -> win/lose
        else:
            if v[2] > w[2]:
                outcome = 'win'
                w_nm = nm1
                l_nm = nm2
            else:
                outcome = 'lose'
                w_nm = nm2
                l_nm = nm1
            if verbose:
                print("So even though both fighters spent time looking for each other, \n%s got lucky and spotted %s first, allowing %s to get in the first tickle.  %s won!" % (w_nm,l_nm,w_nm,w_nm))
                input('\n[Press Enter]: ')
    return outcome

#f2 = random_fighter(8, 1, 2, False); f1 = random_fighter(8, 1, 2, False);
#f1, f2, eval_battle(f1, f2), eval_battle(f1, f2, verbose = False)

def create_fighter():
    print('')
    print('To create a fighter, create a card that has a name and four scores (numbers between 1 and 20): Hiding, Finding, Luck, and Tickling.')
    print('H = how good the fighter is at sneaking around.')
    print('F = how good the fighter is at spotting another sneaky fighter.')
    print('L = how lucky the fighter is.')
    print('T = how good the fighter is at tickling another fighter.')
    print('Each of the scores H, F, L, and T is between 1 and 20 (including 1 and 20; decimals and fractions are allowed in the pen-and-paper version but not in this demo) and together, they have to satisfy the following inequality:')
    print('    (H x F) + (H x L) + (F x T) <= 100.')
    print('This demo will help keep track of the math so that your fighter will comply with this requirement.')
    print('')
    name = input('Name of fighter? ')
    if name == '':
        name = 'Untitled'
    h = int(input('H score [1-20]: '))
    if h < 1:
        h = 1
        print('H was set to 1 to comply with the requirement.')
    if h > 20:
        h = 1
        print('H was set to 20 to comply with the requirement.')
    f = int(input('F score [1-%i]: ' % min(20, int((100 - 1 - h)/h))))
    if f < 1:
        f = 1
        print('F was set to 1 to comply with the requirement.')
    if f > 20:
        f = 20
        print('F was set to 20 to comply with the requirement.')
    l = int(input('L score [1-%i]: ' % min(20, int((100 - f - h*f)/h))))
    if l < 1:
        l = 1
        print('L was set to 1 to comply with the requirement.')
    if f > 20:
        f = 20
        print('F was set to 20 to comply with the requirement.')
    tt = int(input('T score [1-%i]: ' % min(20, int((100 - h*f - h*l)/f))))
    if tt < 1:
        tt = 1
        print('T was set to 1 to comply with the requirement.')
    if tt > 20:
        tt = 20
        print('T was set to 20 to comply with the requirement.')
    print('Let\'s check the formula.  (H x F) + (H x L) + (F x T) = %i + %i + %i = %i.' % (h * f, h*l, f*tt, h*f+h*l+f*tt))
    assert(((h * f) + (h * l) + (f * tt)) <= 100)
    pause = input('Successfully created fighter %s.  Press enter: ' % name)
    fighter = [name, h, f, l, tt]
    return fighter

def formula(v):
    return v[0]*v[1] + v[0]*v[2] + v[1] * v[3]

def random_fighter(upper = 100, lower = 95, base = 19, boost = True):
    v = choice(base, 4) + 1
    while formula(v) > upper or formula(v) < lower:
        v = choice(base, 4) + 1
    if boost:
        if (upper - formula(v)) >= v[1]:
            v[3] = v[3] + 1
        if (upper - formula(v)) >= v[0]:
            v[2] = v[2] + 1
        if (upper - formula(v)) >= (v[0] + v[3]):
            v[1] = v[1] + 1
        if (upper - formula(v)) >= (v[1] + v[2]):
            v[0] = v[0] + 1
    corrs = []
    name_s = []
    for i in range(10):
        name, v2 = random_name_and_stat()
        name_s.append(name)
        corrs.append(np.dot(v, v2)/np.sqrt(1 + np.dot(v,v))/np.sqrt(1 + np.dot(v2,v2)))
    name = name_s[np.argmax(corrs)]
    h, f, l, tt = v
    fighter = [name, h, f, l, tt]
    assert(formula(v) <= upper)
    return fighter


def create_or_choose_fighter(fighters, pp):
    flag = True
    while flag:
        print('')
        print('1. Create a new fighter')
        print('2. Choose an existing fighter')
        print('3. Roll a random fighter')
        print('Q. Quit game')
        flag2 = True
        while flag2:
            inp = input('Please select one: ')
            if inp in ['1', '2', '3', 'Q']:
                flag2 = False
        if inp == 'Q':
            # Cause an error to exit the loop
            return fighters, None
        if inp == '1' or inp=='3':
            if inp=='1':
                fighter = create_fighter()
                print('created')
                flag = False
            if inp=='3':
                flag2 = True
                while flag2:
                    print('\nRolled the following fighter:\n')
                    fighter = random_fighter()
                    print(list_to_fighter_df([fighter]))
                    flag3 = True
                    while flag3:
                        inp = input('Re-roll? (y/n): ')
                        if inp in ['y', 'n']:
                            flag3 = False
                        if inp == 'n':
                            flag2 = False
                flag = False
            if fighters is None:
                print('created DF')
                fighters = pd.DataFrame({'name': [fighter[0]], 'H':[fighter[1]], 'F': [fighter[2]], 'L': [fighter[3]], 'T': [fighter[4]]})
            else:
                print('added to DF')
                fighters.loc[len(fighters.index)] = fighter
        if inp == '2':
            flag2 = True
            while flag2:
                print('')
                print(fighters.iloc[range((pp['page']-1) * pp['per_page'],min(len(fighters), pp['page'] * pp['per_page']))])
                print('n: next page')
                print('p: previous page')
                print('Q: back to menu')
                print('')
                chs = fighters.index.values.astype(str)[(pp['page']-1) * pp['per_page']:min(len(fighters), pp['page'] * pp['per_page'])]
                print('Choices: '+ ', '.join(chs) + ', n, p, Q')
                flag3 = True
                while flag3:
                    inp2 = input('Please select: ')
                    if inp2 in chs or inp2 in ['n', 'p', 'Q']:
                        flag3 = False
                if inp2 == 'n' and pp['per_page']*pp['page'] < len(fighters):
                    pp['page'] = pp['page']+1
                if inp2 == 'p' and pp['page'] > 1:
                    pp['page'] = pp['page']-1
                if inp2 == 'Q':
                    flag2 = False
                if inp2 in chs:
                    ind = np.nonzero(fighters.index.values.astype(str) == inp2)[0][0]
                    fighter = [
                        fighters.iloc[ind, 0],
                        int(np.clip(fighters.iloc[ind, 1], 1, 20)),
                        int(np.clip(fighters.iloc[ind, 2], 1, 20)),
                        int(np.clip(fighters.iloc[ind, 3], 1, 20)),
                        int(np.clip(fighters.iloc[ind, 4], 1, 20))]
                    h,f,l,t = fighters.iloc[ind, 1:5]
                    assert(h*f + f*t + h*l <= 100)
                    flag = False
                    flag2 = False
    return fighters, fighter

def list_to_fighter_df(ls):
    return pd.DataFrame({
        'name': [l[0] for l in ls],
        'H': [l[1] for l in ls],
        'F': [l[2] for l in ls],
        'L': [l[3] for l in ls],
        'T': [l[4] for l in ls]})

def view_game_data(p_fighters, cpu_fighters, inds_p_used, inds_c_used, inds_p_discarded, inds_c_discarded, spoiler = False, display_own = False):
    print('\n\n\n********************************************')
    print('Game data')
    print('')
    inds_c_current = [i for i in range(len(cpu_fighters)) if i not in inds_c_discarded]
    if spoiler and len(inds_c_current) > 0:
        print('Remaining CPU fighters:')
        print(list_to_fighter_df([cpu_fighters[i] for i in np.unique(inds_c_current)]))
    else:
        print('Known CPU fighters:')
        inds_c_seen = [i for i in np.unique(inds_c_used) if i not in inds_c_discarded]
        if len(inds_c_seen) == 0:
            print(' CPU\'s remaining fighters are unknown.')
        else:
            print(list_to_fighter_df([cpu_fighters[i] for i in np.unique(inds_c_seen)]))
        print('')
    print('Discarded CPU fighters:')
    if len(inds_c_discarded) == 0:
        print(' None discarded so far!')
    else:
        print(list_to_fighter_df([cpu_fighters[i] for i in np.unique(inds_c_discarded)]))
    print('\n-------------------------------\n')
    inds_p_current = [i for i in range(len(p_fighters)) if i not in inds_p_discarded]
    if display_own and len(inds_p_current) > 0:
        print('Your fighters:')
        p_current_fighters = [f for (i,f) in enumerate(p_fighters) if i not in inds_p_discarded]
        print(list_to_fighter_df(p_current_fighters))
        print('')
    print('Your discarded fighters:')
    if len(inds_p_discarded) == 0:
        print(' None discarded so far :)')
    else:
        print(list_to_fighter_df([p_fighters[i] for i in inds_p_discarded]))
    input('[Press enter]: ')

def play_game(fighters, pp):
    print('')
    print('Once each player has a team of exactly five fighters, you can play.  Decide who goes first, then start taking turns.  On your turn, choose any other player to challenge to a tickle duel.  Since there\'s only two players in this case, you will be dueling the computer every turn.')
    print('')
    gameflag = True
    cpu_fighters = []
    p_fighters = []
    inds_p_used = []
    inds_p_discarded = []
    inds_c_used = []
    inds_c_discarded = []
    for i in range(5):
        cpu_fighters.append(random_fighter())
    # Setup
    flag = True
    while flag:
        print('')
        if len(p_fighters) > 0:
            print('Current team:')
            print(list_to_fighter_df(p_fighters))
            print('')
        print('Now you need to choose %i fighters:' % (5 - len(p_fighters)))
        try:
            fighters, fighter = create_or_choose_fighter(fighters, pp)
            if fighter is None:
                return fighters
            else:
                p_fighters.append(fighter)
        except:
            print("An error occurred in creating or choosing fighter, please try again.")
        if len(p_fighters) == 5:
            flag = False
    p_eliminated = False
    c_eliminated = False
    print('\nIn a tickle duel, you and your opponent each choose a fighter from your team secretly. After revealing your fighters, play a game of \"hide and find\".')
    print('If you lose the duel, or tie (even after applying tie breakers), you discard your fighter!  If you win the duel, return your fighter to your team.')
    print('You are eliminated from the game when you have no fighters left in your team.  You win if you eliminate all other players.')
    print('The computer will use randomly generated fighters.')
    input('[Press enter]: ')
    p_create_at_2 = False
    p_create_at_4 = False
    c_create_at_2 = False
    c_create_at_4 = False
    displayed_midgame_rule = False
    while gameflag:
        # mid-game creation
        if (len(inds_p_discarded)==2 and not p_create_at_2) or (len(inds_p_discarded)==4 and not p_create_at_4):
            if not displayed_midgame_rule:
                print('After you have lost two fighters, you add another card to your team, either by making a new one, or taking one you already made from before (other than a discarded fighter).  Do the same after losing 4 fighters.')
                displayed_midgame_rule = True
            if len(inds_p_discarded) == 2:
                print('You just lost 2 fighters, so you create a new one.')
                p_create_at_2 = True
            if len(inds_p_discarded) == 4:
                print('You just lost 4 fighters, so you create a new one.')
                p_create_at_4 = True
            input('[Press enter]: ')
            flag = True
            while flag:
                print('')
                view_game_data(p_fighters, cpu_fighters, inds_p_used, inds_c_used, inds_p_discarded, inds_c_discarded, False, True)
                print('')
                try:
                    fighters, fighter = create_or_choose_fighter(fighters, pp)
                    if fighter is None:
                        return fighters
                    else:
                        p_fighters.append(fighter)
                        flag = False
                except:
                    print("An error occurred in creating or choosing fighter, please try again.")
        if (len(inds_c_discarded)==2 and not c_create_at_2) or (len(inds_c_discarded)==4 and not c_create_at_4):
            if not displayed_midgame_rule:
                print('After you have lost two fighters, you add another card to your team, either by making a new one, or taking one you already made from before (other than a discarded fighter).  Do the same after losing 4 fighters.')
                displayed_midgame_rule = True
            if len(inds_c_discarded) == 2:
                print('CPU just lost 2 fighters, so CPU creates a new one.')
                c_create_at_2 = True
            if len(inds_c_discarded) == 4:
                print('CPU just lost 4 fighters, so CPU creates a new one.')
                c_create_at_4 = True
            cpu_fighters.append(random_fighter())
            input('[Press enter]: ')
        flag = True
        p_current_fighters = [f for (i,f) in enumerate(p_fighters) if i not in inds_p_discarded]
        inds_p_current = [i for i in range(len(p_fighters)) if i not in inds_p_discarded]
        while flag:
            print('')
            if not p_eliminated and not c_eliminated:
                print('Choose a fighter from your team to send out!')
                print('')
                print(list_to_fighter_df(p_current_fighters))
                ch_str = 'choices: ' + ', '.join([str(i) for i in range(len(p_current_fighters))]) + ', ?, Q'
            else:
                print('Game over!')
                print('+: Add computer fighters to collection')
                ch_str = 'choices: ?, +, Q'
            print('?: view game data')
            print('Q: quit game')
            print(ch_str)
            inp = input('Please choose one: ')
            if inp == '+' and (p_eliminated or c_eliminated):
                fighters = pd.concat([fighters, list_to_fighter_df(cpu_fighters)], axis = 0)
                fighters.reset_index(drop=True, inplace=True)
                print('')
                print(fighters)
                input('\nAdded!\n[Press enter]:')
            if inp in [str(i) for i in range(len(p_current_fighters))]:
                flag = False
                inds_p_used.append(inds_p_current[int(inp)])
                pl_choice = inds_p_current[int(inp)]
            if inp == 'Q':
                return fighters
            if inp == '?':
                if c_eliminated or p_eliminated:
                    view_game_data(p_fighters, cpu_fighters, inds_p_used, inds_c_used, inds_p_discarded, inds_c_discarded, True, True)
                else:
                    view_game_data(p_fighters, cpu_fighters, inds_p_used, inds_c_used, inds_p_discarded, inds_c_discarded)
        # CPU chooses
        inds_c_current = [i for i in range(len(cpu_fighters)) if i not in inds_c_discarded]
        cp_choice = inds_c_current[int(choice(len(cpu_fighters) - len(inds_c_discarded), 1))]
        inds_c_used.append(cp_choice)
        # resolve fight
        outcome = eval_battle(p_fighters[pl_choice], cpu_fighters[cp_choice])
        if outcome == 'win' or outcome == 'draw':
            print('CPU fighter %s is discarded!' % cpu_fighters[cp_choice][0])
            inds_c_discarded.append(cp_choice)
            input('[Press enter]: ')
        if outcome == 'lose' or outcome == 'draw':
            print('Your fighter %s is discarded!' % p_fighters[pl_choice][0])
            inds_p_discarded.append(pl_choice)
            input('[Press enter]: ')
        p_eliminated = (len(p_fighters) - len(inds_p_discarded)) == 0
        c_eliminated = (len(cpu_fighters) - len(inds_c_discarded)) == 0
        if p_eliminated and not c_eliminated:
            print('\nYou have no more fighters remaining! You lose!')
            input('[Press enter]: ')
        if c_eliminated and not p_eliminated:
            print('\nCPU has no more fighters remaining! You win!')
            input('[Press enter]: ')
        if p_eliminated and c_eliminated:
            print('\nIt\'s a draw!')
            input('[Press enter]: ')
    return fighters

# page params
pp = {'page' : 1, 'per_page' : 20}

while mainflag:
    print('')
    print('Menu:')
    print('1. Play vs computer')
    print('2. Create fighters')
    print('3. Roll a random fighter')
    print('4. Edit fighters')
    print('5. Save current fighters to file')
    print('6. Load a fighter file')
    print('7. Options')
    print('Q. Exit')
    flag = True
    while flag:
        inp = input('Please select one: ')
        if inp in ['1', '2', '3', '4', '5', '6', '7', 'Q']:
            flag = False
    if inp == '7':
        try:
            pp['per_page'] = int(input('Set fighters per page: '))
            pp['per_page'] = int(np.clip(pp['per_page'], 2, 9999999))
        except:
            input('Error! [Press enter]:')
    if inp == '4':
        if fighters is None:
            print('First create some fighters, or load a file!')
        else:
            flag2 = True
            while flag2:
                print('')
                print(fighters.iloc[range((pp['page']-1) * pp['per_page'],min(len(fighters), pp['page'] * pp['per_page']))])
                print('n: next page')
                print('p: previous page')
                print('Q: back to menu')
                print('')
                chs = fighters.index.values.astype(str)[(pp['page']-1) * pp['per_page']:min(len(fighters), pp['page'] * pp['per_page'])]
                print('Choices: '+ ', '.join(chs) + ', n, p, Q')
                flag3 = True
                while flag3:
                    inp2 = input('Please select: ')
                    if inp2 in chs or inp2 in ['n', 'p', 'Q']:
                        flag3 = False
                if inp2 == 'n' and pp['per_page']*pp['page'] < len(fighters):
                    pp['page'] = pp['page']+1
                if inp2 == 'p' and pp['page'] > 1:
                    pp['page'] = pp['page']-1
                if inp2 == 'Q':
                    flag2 = False
                # edit fighter
                if inp2 in chs:
                    flag3 = True
                    while flag3:
                        try:
                            print('\n')
                            f_ind = np.nonzero(fighters.index.values.astype(str) == inp2)[0][0]
                            print(fighters.iloc[[f_ind]])
                            print("\nEdit menu\nN. Name\nH. HIDE\nF. FIND\nL. LUCK\nT. TICKLE\nI. (Switch) Index\nD. Delete fighter\nB. Back to edit")
                            flag4 = True
                            while flag4:
                                h, f, l, t = fighters.iloc[f_ind, 1:5]
                                inp3 = input('Please select (N, H, F, L, T, D, B): ')
                                if inp3 in ['N', 'H', 'F', 'L', 'T', 'D', 'B']:
                                    flag4 = False
                                if inp3 == 'B':
                                    flag3 = False
                                if inp3 == 'N':
                                    fighters.iloc[f_ind, 0] = input('Please enter new name: ')
                                if inp3 == 'I':
                                    flag4 = False
                                    new_ind = int(input('Please enter index to switch with: '))
                                    fighters.rename(index = {fighters.index[f_ind]: new_ind, new_ind: fighters.index[f_ind]}, inplace=True)
                                    fighters = fighters.iloc[np.argsort(fighters.index.values)]
                                    fighters.reset_index(inplace=True, drop = True)
                                    flag3 = False
                                if inp3 == 'H':
                                    max_h = min(20, int((100 - f*t)/(l + f)))
                                    h = int(input('H score [1-%i]: ' % max_h))
                                    if h < 1:
                                        h = 1
                                    if h > max_h:
                                        h = max_h
                                    fighters.iloc[f_ind, 1] = h
                                if inp3 == 'F':
                                    max_f = min(20, int((100 - h*l)/(t + h)))
                                    f = int(input('F score [1-%i]: ' % max_f))
                                    if f < 1:
                                        f = 1
                                    if f > max_f:
                                        f = max_f
                                    fighters.iloc[f_ind, 2] = f
                                if inp3 == 'L':
                                    max_l = min(20, int((100 - h*f - f*t)/h))
                                    l = int(input('L score [1-%i]: ' % max_l))
                                    if l < 1:
                                        l = 1
                                    if l > max_l:
                                        l = max_l
                                    fighters.iloc[f_ind, 3] = l
                                if inp3 == 'T':
                                    max_t = min(20, int((100 - h*f - h*l)/f))
                                    t = int(input('T score [1-%i]: ' % max_t))
                                    if t < 1:
                                        t = 1
                                    if t > max_t:
                                        t = max_t
                                    fighters.iloc[f_ind, 4] = t
                                if inp3 == 'D':
                                    inp4 = input('Delete fighter: Are you sure (y/n)?: ')
                                    if inp4 != 'n':
                                        flag4 = False
                                        flag3 = False
                                        fighters = fighters.drop([fighters.index[f_ind]])
                                        if ((pp['page']-1) * pp['per_page']+1) > len(fighters):
                                            pp['page'] = pp['page'] - 1
                                        fighters.reset_index(inplace=True, drop = True)
                        except:
                            input("Error! Press enter: ")

    if inp == 'Q':
        mainflag = False
        print('\nThank you for playing the demo for Hide-and-Find Tickle Fight!')
        print('Game created by Charles Zheng and Helen Xu in 2023.  This game, demo, and associated files (names.txt, adjectives.txt, nouns.txt) are considered public domain under the CC0 license (https://creativecommons.org/share-your-work/public-domain/cc0/).')
    if inp == '5':
        fn = input('filename: ')
        try:
            fighters.to_csv(fn)
            pause = input('Success!  Press enter: ')
        except:
            print('error saving!')
            pause = input('Press enter: ')
    if inp == '6':
        fn = input('filename: ')
        try:
            fighters = pd.read_csv(fn, index_col = 0)
            print('')
            print(fighters)
            print('')
            pp['page'] = 1
            pause = input('Success!  Press enter: ')
        except:
            print('error loading!')
            pause = input('Press enter: ')
    if inp == '2' or inp == '3':
        try:
            if inp=='2':
                fighter = create_fighter()
            if inp=='3':
                flag2 = True
                while flag2:
                    print('\nRolled the following fighter:\n')
                    fighter = random_fighter()
                    print(list_to_fighter_df([fighter]))
                    flag3 = True
                    while flag3:
                        inp = input('Re-roll? (y/n): ')
                        if inp in ['y', 'n']:
                            flag3 = False
                        if inp == 'n':
                            flag2 = False
            if fighters is None:
                fighters = pd.DataFrame({'name': [fighter[0]], 'H':[fighter[1]], 'F': [fighter[2]], 'L': [fighter[3]], 'T': [fighter[4]]})
            else:
                fighters.loc[len(fighters.index)] = fighter
        except:
            print('An error occurred in creating a fighter.  Please try again.')
    if inp == '1':
        fighters = play_game(fighters, pp)
