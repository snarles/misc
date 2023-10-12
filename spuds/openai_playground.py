import os
import openai

openai.api_key = os.getenv("OPENAI_API_KEY")

response = openai.ChatCompletion.create(
  model="gpt-3.5-turbo",
  messages=[
    {
      "role": "user",
      "content": "I am writing a story where food items are brought to life.  Here are the food items that I am using:\n\n1. French fries\n2. Potato chips\n3. Baked potato\n\nFor each food item, please help me come up with some details about my character based on that food item.  Please decide which gender and sexual orientation they are, and come up with at least 10 quirks or personality traits.  These attributes should be relatable to facts about the food item, but please keep the explanations short--just 20 words or fewer for each trait.  Include both positive and negative qualities."
    },
    {
      "role": "assistant",
      "content": "1. French Fries:\nCharacter: Male character named Frankie Fry, who identifies as gay.\n\nPersonality Traits:\n1. Crispy Confidence: Bold and self-assured, Frankie exudes confidence in any situation.\n2. Sizzling Sense of Humor: Known for his quick wit and infectious laughter.\n3. Golden Loyalty: Devoted and steadfast, Frankie is a loyal friend.\n4. Salty Sass: Sometimes, he can be a bit sarcastic or snarky.\n5. Flavorful Adventures: Always up for trying new things and seeking exciting experiences.\n6. Grease Monopoly: Frank can be possessive and overprotective at times.\n7. Fiery Temper: His enthusiasm can quickly turn into anger when things don't go his way.\n8. Crinkle-cut Creativity: Frankie has a unique and imaginative perspective.\n9. Needs Dip: He can be reliant on others or seek validation excessively.\n10. PotatOHolic: Sometimes, he struggles with moderation and can become obsessive about certain things.\n\n2. Potato Chips:\nCharacter: Female character named Patty Chip, who identifies as bisexual.\n\nPersonality Traits:\n1. Snack Pack Charm: Patty is irresistibly charming, a natural at winning people over.\n2. Crispy Versatility: Adaptable and flexible, she effortlessly adjusts to different situations.\n3. Bold Flavorful Opinions: She's not afraid to voice her thoughts and beliefs, sometimes leading to conflicts.\n4. Satisfying Curiosity: Patty is always eager to learn and explore new things.\n5. Crumbly Insecurities: Despite her outward confidence, she can struggle with self-doubt.\n6. Crunchy Ambition: She's driven to achieve her goals and has a competitive nature.\n7. Laid-Back Layers: Underneath her go-getter attitude, Patty can be relaxed and easygoing.\n8. Grease Fetish: Patty occasionally indulges in guilty pleasures to an excessive degree.\n9. Ripple Effect: Her actions and decisions can have far-reaching consequences without her realizing it.\n10. Shareable Nature: She's generous and loves to share, but can also be possessive over certain things she holds dear.\n\n3. Baked Potato:\nCharacter: Non-binary character named Sam Spud, who identifies as pansexual.\n\nPersonality Traits:\n1. Hearty Warmth: Sam exudes a comforting and nurturing presence, always ready to lend an ear.\n2. Sturdy Resilience: They're reliable and handle challenges with grace and determination.\n3. Soft-Centered Sensitivity: Sam is deeply empathetic but can also be easily hurt.\n4. Skin-Deep Modesty: They're humble and often dismiss praise or recognition for their achievements.\n5. Creative Toppings: Always thinking outside of the box, Sam brings a unique perspective to every situation.\n6. Slow-Baked Patience: They have a calm demeanor and remarkable patience, even in stressful situations.\n7. Nutritional Awareness: Sam is knowledgeable about self-care and encourages healthy habits in themselves and others.\n8. Eyes on the Peel: Can be overly cautious, sometimes missing out on opportunities due to fear.\n9. Multilayered Identity: Sam struggles with self-acceptance and revealing their authentic self to others.\n10. Internal Heat: Despite their gentle nature, Sam can be prone to outbursts when overwhelmed or provoked."
    },
    {
      "role": "user",
      "content": "For each of the above 3 characters, please summarize their personality into a 6-number summary.  Each of the 6 numbers is from 1 to 100.  The first number is the character's deceptiveness or shyness.  The second number is their ability to empathize and understand other people.  The third number is their ability to impress strangers.  The fourth number is the intensity of their passion or desire.  The fifth number is their extent of knowledge.  The sixth number is their efficiency at doing tasks, their reactiveness to surprises and their intelligence at learning new things.  Please give the 6-number summaries for the 3 characters as a numbered list, where each line of the list contains only the 6 numbers separated by commas."
    },
    {
      "role": "assistant",
      "content": "Here is the 6-number summary for each of the three characters:\n\n1. Frankie Fry (French Fries):\n54, 78, 82, 72, 60, 69\n\n2. Patty Chip (Potato Chips):\n68, 81, 77, 87, 64, 75\n\n3. Sam Spud (Baked Potato):\n42, 89, 65, 78, 74, 83"
    }
  ],
  temperature=1,
  max_tokens=148,
  top_p=1,
  frequency_penalty=0,
  presence_penalty=0
)















import os
import openai

openai.api_key = os.getenv("OPENAI_API_KEY")

response = openai.ChatCompletion.create(
  model="gpt-3.5-turbo",
  messages=[
    {
      "role": "user",
      "content": "Please write a review about this dish:  Chocolate-covered potato chips\n\nFirst, describe the restaurant in 30 words or fewer.  How did the ambience make you feel?  Did you feel classy, or down-to-earth?  Ready for fun, or sophisticated, or thoughtful?\n\nNext, describe the appearance of the dish and the feelings that it inspired.  Finally, write about the taste of the dish.  Describe each individual ingredient that you tasted and how it combined with others.  Mention the temperature, texture, and the flavor.  How did it make you feel?\n\nFinally, imagine become the dish itself in a mystical experience.  How does it feel to be that dish?  If that dish became alive, what personality traits or quirks would it have?  Include both positive and negative traits, and link these traits to the feelings you write about earlier, including the restaurant, the appearance of the dish, and the taste of the dish.\n"
    }
  ],
  temperature=1,
  max_tokens=1535,
  top_p=1,
  frequency_penalty=0,
  presence_penalty=0
)
