---
hide:
  - navigation
  - toc
---

# Oxygen

Oxygen is a group of scala libraries aimed to make everyday tasks in scala easier.

!!! abstract "Mission Statement"

    The mission statement of `Oxygen` is to make it easy and accessible for anyone who knows scala to both develop and deploy full-stack applications using `nothing but scala`, and requiring only `Oxygen` as a direct dependency.

## The story of Oxygen

The story of how `Oxygen` came to be is a very personal one, so I will start with a little bit about me.

I, [Kalin Rudnicki](https://www.linkedin.com/in/kalin-rudnicki-5ab151150/), am a software engineer with 5 years of professional backend scala development.
I am very passionate about programming. I started my software development journey on my `TI-84 calculator` in math class, and haven't looked back since.

### Beginnings in Ruby

In college, I got my first internship using Ruby.
As I learned Ruby at work, I naturally wanted to use it to try and implement all the fun side project ideas I had floating around in my head.
With chronic ADHD, I would jump back and forth from one idea to another, and realized I started repeating myself, a lot...
And thus, the idea of `"my shared library"` began; enter: `KLib`.
Any time I was working on one of my `"side project ideas"`, and something seemed like I might want to use it again, I would toss it in `KLib`.

### Let the Scala Begin

If Ruby was my first love, then Scala is my forever girl.

After graduation, I took my first job writing Scala, having never used it before, learning on the job.
As I learned Scala at work, I naturally wanted to use it to try and implement all the fun side project ideas I had floating around in my head.  
So, what do I do... `KLib`, but in Scala... (Obviously)

Over the following year or two, I added a plethora of things to `KLib`, and the garbage pile, sorry, the library, grew.
Being so new and inexperienced, the majority of the code was terrible and inconsistent, and I couldn't look back at what I did a month before and have any clue what I was thinking.
It had reached a point where the majority of the code I had written for `KLib` was not even usable at a `"personal project level"`.

### Time for Some Security

At this point, I was very into rock climbing, and you never go climbing without your `Harness`.
I never went anywhere without `"my shared library"`, so the name just fit.

`Harness` was a total rewrite, from scratch, with two years of mistakes to guide the way.
This time, the goal: I want something that's good enough to be usable at a `"personal project level"`.

It needed to be good enough that I could come back to it a month later and have at least some idea what the heck I was thinking when I wrote it.  
It needed to be good enough that it was helping me get things done on the average ADHD side-quest.  
And it was... for a while...

You could do a lot with `Harness`... To be fair, it was full-stack...
You could build a UI, web server, and interact with the database, all using `Harness`.
You might hate your life, but you could do it.  
`Harness` eventually reached a point where I knew what that code I wrote a year ago meant, but in hindsight, there was just a much better way to do it.
And yes, while it was an amazing learning experience to write that web server from scratch, there's no way that thing is ever going to be production ready.
It was indeed good enough for my personal projects, but would I ever recommend using it in production.... not a chance...

### A Breath of New Life

You might take your climbing harness with you every time you go climbing, but you can't breathe without `Oxygen`.

With four years into professional development experience, and lots of lessons to go with it, it was time to make it official.
The bar to meet for `Oxygen`: I need to be comfortable recommending we use this at work, in production, with millions of dollars on the line.  
And thus, another library is born from the ashes. A bright-eyed, bushy-tailed new repo, but hardened with the ancestral knowledge of those that came before.  
Long live `KLib` and `Harness`, but we're goin to prod baby...

So, what does `Oxygen` actually do...?  
The core bits are [oxygen-sql](sql/index.md), [oxygen-http](http/index.md), and [oxygen-ui](ui/index.md).
Sure, libraries for all these things exist, but do they play nicely with each other? Like... at all?

- Sure, `doobie`'s `ConnectionIO` can work with `Task`, but what happens when I want to execute an effect (even as simple as logging), in the middle of a transaction. Guess what... you cant...
- Writing SQL from hand in scala... why...
- Why am I writing all my DB models in scala, deriving codecs for them, but still writing my flyway migrations by hand...
- Sure `tapir` works with `zio-http` and `zio-json`, but they are using different derivation annotations under the hood, and `tapir` can't represent some of the things `zio-json` can do... at all...  
  What am I supposed to tell our FE engineers when they complain that the docs don't match reality? "Sorry, our schema library sucks."?

These are the things that `Oxygen` aims to address.
All the way from your `<div>` to your `database`, it's straight up `zio`, and each link in the chain is intentionally crafted to play nicely with the next.
The core principles of `Oxygen` are:

1. --- Compatibility ---  
   All the parts of your application are working for you in unison.
2. --- Minimal Boilerplate ---  
   If it can be derived, it is... If another library has you defining 4 different constructs, we do it 3, 2, or sometimes even 1. 
3. --- Easy Startup ---  
   I have been using doobie at work for 4 years now. There is no way I could set up a doobie integration without copying some nasty nasty `zTransactor.scala`.  
   Same thing with tapir endpoints.  
   The core bits I work on every day have me longing for more, but if I needed to touch the bits at the root tying all these things together, I'd need a gas mask...  
   Not with `Oxygen`... Because the entry points to `Oxygen` modules are intentionally built to be simple, with as few moving parts, so getting started is a piece of cake.
4. --- Works First Try ---  
   This is really a product of the above principles, but when everything flows nicely, and is simple at its core, it's really easy to write code that works exactly as you'd expect, first try.
