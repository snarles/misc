#!/usr/bin/env python
# coding: utf-8

from llama import Llama
import os
import time
import torch
import subprocess
import numpy as np


max_gen_len = None
temperature = 1.0
top_p = 0.9

#os.environ["CUDA_VISIBLE_DEVICES"]=""

llama_home = '/lscratch/llama/'
ckpt_dir = llama_home + 'llama-2-70b-chat'
tokenizer_path = llama_home + 'tokenizer.model'
max_seq_len = 1024
max_batch_size = 4

generator = Llama.build(
    ckpt_dir=ckpt_dir,
    tokenizer_path=tokenizer_path,
    max_seq_len=max_seq_len,
    max_batch_size=max_batch_size,
)

B_INST, E_INST = "[INST]", "[/INST]"
B_SYS, E_SYS = "<<SYS>>\n", "\n<</SYS>>\n\n"

SPECIAL_TAGS = [B_INST, E_INST, "<<SYS>>", "<</SYS>>"]
UNSAFE_ERROR = "Error: special tags are not allowed as part of the prompt."

max_gen_len = 512

def generate_response(dialog):
    if dialog[0]["role"] == "system":
        dialog = [
            {
                "role": dialog[1]["role"],
                "content": B_SYS
                + dialog[0]["content"]
                + E_SYS
                + dialog[1]["content"],
            }
        ] + dialog[2:]
    dialog_tokens = sum(
        [
            generator.tokenizer.encode(
                f"{B_INST} {(prompt['content']).strip()} {E_INST} {(answer['content']).strip()} ",
                bos=True,
                eos=True,
            )
            for prompt, answer in zip(
                dialog[::2],
                dialog[1::2],
            )
        ],
        [],
    )
    assert(all([msg["role"] == "user" for msg in dialog[::2]]))
    assert(all([msg["role"] == "assistant" for msg in dialog[1::2]]))
    assert(dialog[-1]["role"] == "user")
    dialog_tokens += generator.tokenizer.encode(
                    f"{B_INST} {(dialog[-1]['content']).strip()} {E_INST}",
                    bos=True,
                    eos=False,
                )
    prompt_tokens = [dialog_tokens]
    logprobs = False
    generation_tokens, generation_logprobs = generator.generate(
        prompt_tokens=prompt_tokens,
        max_gen_len=max_gen_len,
        temperature=temperature,
        top_p=top_p,
        logprobs=logprobs,
    )
    ans = [ generator.tokenizer.decode(t) for t in generation_tokens ]
    return ans[0]

# generate prompt

adjs = np.loadtxt("talkenglish_adj_list.txt", dtype = str)


def generate_prompt(adj):
    return("Which of the following sentences is a more typical?\n\n1. (a) She hid her true feelings because she is a ADJ person.  (b) She didn't hide her true feelings because she is a ADJ person.\n2. (a) He was able to disguise his intentions to her, because he is a ADJ person.  (b) He was not able to disguise his intentions to her, because he is a ADJ person.\n3. (a) He didn't show his feelings on his face--he is a ADJ person.  (b) He couldn't help but show his feelings on his face--he is a ADJ person.\n4. (a) She didn't feel confident to speak up, because she is a ADJ person.  (b) She did feel confident to speak up, because she is a ADJ person.\n5. (a) She was able to infiltrate the enemy camp and pose as one of their own, because she is such a ADJ person.  (b) She tried to infiltrate the enemy camp but was discovered, because she is such a ADJ person.\n6. (a) He could tell she was lying, because he is a ADJ person.  (b) He couldn't tell that she was lying, because he is a ADJ person.\n7. (a) Few could tell what he was trying to communicate through hand gestures, but she could, because she is a ADJ person.  (b) She couldn't tell what he was trying to communicate through hand gestures, because she is a ADJ person.\n8. (a) He called her bluff, and she thought, what a ADJ person.  (b) He couldn't tell she was bluffing, and she thought, what a ADJ person.\n9. (a) She remembered meeting him last time, and could predict what he was going to do, since she is a ADJ person.  (b) She forgot that she already met him before, and what he did surprised her, since she is a ADJ person.\n10. (a) She was good at telling when someone was lying, she was a ADJ person.  (b) She wasn't good at telling when someone was lying, she was a ADJ person.\n11. (a) She enchanted him completely, because she is a ADJ person.  (b) She failed to catch his attention, because she is a ADJ person.\n12. (a) She wanted to imitate him because he is a ADJ person.  (b) She didn't want to imitate him at all, because he is a ADJ person.\n13. (a) He felt attracted to her, because she is a ADJ person.  (b) He didn't feel attracted to her, because she is a ADJ person.\n14. (a) He felt tremendous respect for her, and thought, what a ADJ person.  (b) He didn't feel much respect for her, and thought, what a ADJ person.\n15. (a) She wasn't repulsed by that man, because he seemed like a ADJ person.  (b) She was repulsed by that man, because he seemed like a ADJ person.\n16. (a) She feels very strongly about her cause, for she is quite a ADJ person.  (b) She doesn't care that much about her cause, for she is quite a ADJ person.\n17. (a) His speech was very compelling, because he was a ADJ person.  (b) His speech wasn't very compelling, because he was a ADJ person.\n18. (a) He worked very hard every day, because he is a ADJ person.  (b) He usually didn't work hard, because he is a ADJ person.\n19. (a) They say she is one of the most committed team members, that she is really a ADJ person.  (b) They say that she is not particularly committed to the job, that she is really a ADJ person.\n20. (a) He's a really passionate guy, a really ADJ person.  (b) He just lacks passion, a really ADJ person.\n21. (a) He knows a lot because he is a ADJ person.  (b) He knows very little because he is a ADJ person.\n22. (a) She was willing to study for days to gain the knowledge she needed, because she is a ADJ person.  (b) She didn't study much, even though she needed to, because she is a ADJ person.\n23. (a) He is not OK with his ignorance, because he is a ADJ person.  (b) He is OK with not knowing much, because he is a ADJ person.\n24. (a) She knows so much, it must be because when she has always been such a ADJ person.  (b) She knows surprisingly little, it must be because when she has always been such a ADJ person.\n25. (a) He got the best score on the test easily, they say he is a really ADJ person.  (b) He struggled on the test, so they say he is a really ADJ person.\n26. (a) She cooked an entire week's worth of meals in one day, demonstrating that she is a ADJ person.  (b) She took so long to prepare just one simple meal, showing that she is a ADJ person.\n27. (a) He quickly thought of a witty comeback to her insult, and she thought, what a ADJ person.  (b) He was taken aback at her insult, and she thought, what a ADJ person.\n28. (a) He is extremely busy, doing the work of three people by himself--what a ADJ person.  (b) He takes a long time to do things, what a ADJ person.\n29. (a) She can't sit still and is always doing things--what a ADJ person.  (b) She just sits in a chair and thinks, and doesn't like to take action--what a ADJ person.  \n30. (a) He was reacting to his opponent's every move, because he was a ADJ person.  (b) He was struggling to keep up with his opponent, because he was a very person.\n\nPlease answer \"a\" or \"b\", no explanation needed.".replace("ADJ", adj))

for adj_i in range(5):
    adj = adjs[adj_i]
    dialog = [
            {"role": "system", "content": generate_prompt(adj)},
    ]
    print(adj)
    print(generate_reponse(dialog))
