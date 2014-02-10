\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #128
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
	\partial 4
  d8 e |
  fis4 d4. b'8\rest e,8 fis |
  g4 e4. b'8\rest cis8 b |
  a4 g fis e |
  d'2. \bar"" cis8 d |
  e4 d a fis |
  cis' cis2 b8 cis |
  
  %page2
  d4 cis b fis |
  a2. \bar"" d,8 e |
  fis4 d4. b'8\rest e,8 fis |
  
  g4 e4. b'8\rest cis8 b |
  a4 g fis e |
  d'2. \bar"" cis8 d |
  
  e4 d a fis |
  cis' cis2 cis8 b |
  a4 ais b cis |
  d2. \bar"||" \break
  
  
  %Chorus
  fis,8 a |
  d4 cis b a |
  fis d4. b'8\rest gis8 a |
  
  b a gis a b a g fis |
  a4 e2 a8 b |
  cis4. b8 cis4. b8 |
  
  a4 a cis4. b8 |
  a4 g fis e |
  fis8[ e]( d4) b'\rest fis8 a |
  
  %page4
  d4 cis b a |
  fis d4. b'8\rest gis8( a) |
  
  b a gis a b a g fis |
  a4 e2 a8 b |
  
  cis4. b8 cis4. b8 |
  a4 a cis4. b8 |
  a4 g fis e |
  d'2. \bar"|."
  
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	In our school -- days, mer -- ry school -- days,
  We were hap -- py girls and boys;
  We would al -- ways play to -- geth -- er,
  And our life was full of joys;
  
  And at \once \override LyricHyphen #'minimum-distance = #0.7 play -- time, in the \once \override LyricHyphen #'minimum-distance = #0.7 May -- time,
  You and I were not a -- part;
  I was then your \once \override LyricHyphen #'minimum-distance = #0.7 school -- boy lov -- er,
  You, my lit -- tle girl sweet -- heart.
  
  %Chorus
  We were go -- ing to be mar -- ried,
  To be, to be, to be, to be, to be mar -- ried,
  When we old -- er grew and bold -- er,
  Then a lit -- tle while we tar -- _ ried,
  
  When I missed you I was lone -- ly,
  For I loved you, Oh! I loved you on -- ly, on -- ly,
  I was then your \once \override LyricHyphen #'minimum-distance = #0.7 school -- boy hus -- band,
  And you were my \once \override LyricHyphen #'minimum-distance = #0.7 school -- girl wife.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Soon we mar -- ried, you and I, dear,
  You to me and I to you,
  And we had a lit -- tle home, dear,
  With just room e -- nough for two;
  
  And a lit -- tle, lat -- er on, dear,
  Still more hap -- py we would be,
  For we found our ti -- ny cot -- tage,
  Was a -- bout the size for three.
  
  %chorus
  It’s de -- light -- ful to be mar -- ried!
  To be, to be, to be, to be, to be mar -- ried!
  There is noth -- ing half so jol -- ly, As a hap -- py wed -- ded life; __ _ _
  And I loved to play with ba -- by,
  Our __ _ ti -- ny lit -- tle, pret -- ty lit -- tle ba -- by,
  I was Pa -- pa, you were Ma -- ma,
  Such a charm -- ing fam -- i -- ly.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  When old age comes, to us both dear,
  We will still be in the game;
  I will be a gay old par -- ty,
  You will be a grand old dame;
  
  And then arm in arm, to -- geth -- er,
  We will go to church right near,
  You will call me your old dar -- ling,
  I will call you my old dear.
  
  %Chorus
  It’s de -- light -- ful to be mar -- ried!
  To be, to be, to be, to be, to be mar -- ried!
  For the heart won’t be un -- ru -- ly,
  If it real -- ly loves one tru -- _ ly;
  
  And your life will not be lone -- ly,
  For I’ll love you, I will love you on -- ly, on -- ly,
  I will be your lov -- ing hus -- band,
  You will be my lov -- ing wife.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    d8 a |
    d4 d4. s8 cis8 d |
    cis4 cis4. s8 e8 e |
    e4 cis cis cis |
    d2. a8 fis' |
    d4 d fis d |
  }
  \alternative {
    {
      e e2 cis8 e |
      fis4 e dis bis |
      cis2.
    }
    {
      e4 e2 e8 cis |
      cis4 cis g' g |
      fis2. \bar"||"
    }
  }
  \repeat unfold 1 {
    d8 d |
    fis4 fis fis fis |
    d d4. s8 f fis |
    f fis f fis f fis g d |
    e4 cis2 e8 g |
    g4. g8 e4. g8 |
    g4 g e4. d8 |
    e4 e cis a |
  }
  \alternative {
    {
      d4( a) s4
    }
    {
      d2. \bar"|."
    }
  }
    d8 d |
    fis4 fis fis fis |
    d d4. s8 f( fis) |
    f fis f fis f fis g d |
    e4 cis2 e8 g |
    g4. g8 e4. g8 |
    g4 g e4. d8 |
    e4 e cis a |
    d2. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  %%
  \repeat unfold 2 {
    fis,8 g |
    a4 a4. s8 a8 a |
    a4 a4. s8 a8 g |
    g4 a fis g |
    fis2. g8 a |
    a4 a d a |
  }
  \alternative {
    {
      a g2 g8 g |
      a4 a fis a |
      a2.
    }
    {
      a4 g2 g8 g |
      a4 g g g |
      a2. \bar"||"
    }
  }
  
  \repeat unfold 1 {
    a8 fis |
    a4 a a a |
    a a4. s8 b8 a |
    gis a b a gis a b a |
    cis4 a2 cis8 cis |
    cis4 a a a |
    cis cis a4. b8 |
    cis4 a a g |
  }
  \alternative {
    {
      a8[ g]( fis4) s
    }
    {
      fis2.
    }
  }
    a8 fis |
    a4 a a a |
    a a4. s8 b8( a) |
    gis a b a gis a b a |
    cis4 a2 cis8 cis |
    cis4 a a a |
    cis cis a4. b8 |
    cis4 a a g |
    fis2.
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    d,8 cis |
    d4 fis4. d8\rest g fis |
    e4 a,4. d8\rest a e' |
    cis[ b] a4 a a 
    d2. e8 d |
    fis4 fis d d |
  }
  \alternative {
    {
      a a2 a8 a |
      d4 d b dis |
      e2.
    }
    {
      a,4 a2 a8 a |
      e'4 e a, a |
      d2. \bar "||"
    }
  }
  
  \repeat unfold 1 {
    d8 d |
    d4 fis d d |
    d fis4. d8\rest d8 d |
    d d d d d d d d |
    a'4 g2 a8 a |
    a4 a g4. g8 |
    a4 a a, a |
    a cis a cis |
  }
  \alternative {
    {
      d4~ d d4\rest 
    }
    {
      d2. \bar"|."
    }
  }
    d8 d |
    d4 fis d d |
    d fis4. d8\rest d8~ d |
    d d d d d d d d |
    a'4 g2 a8 a |
    a4 a g4. g8 |
    a4 a a, a |
    a cis a cis |
      d2. \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"It’s Delightful to be Married!"}}
  poet = \markup\oldStyleNum"Anna Held (1872–1918)"
  composer = \markup\oldStyleNum"Vincent Scotto (1874–1952)"
  tagline = ""
}}
