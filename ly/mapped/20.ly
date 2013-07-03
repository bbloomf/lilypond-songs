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
       (padding . -3)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 60))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #20
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
  \key g \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat volta 2 {
    c'4. d8 c4 a |
    b2 b |
    c4 b a g |
    a2 a |
    b8. c16 d8 b c4 d |
    
    e2 d4 c~ |
    c b a b8 c |
    d4. b8 c4 a |
  }
  \alternative { { b1 } { b2 r4 d }}

  \repeat volta 2 {
    d4. e8 d4 d |
    e2 r4 e |
    
    %page2
    d4. d8 c4 b |
    a1 |
    a4 a8 b a4 a |
    g2 g4 g8 fis |
    e4 d e e |
    
    fis2 fis4 f |
    e e f e |
    e2 e4 e |
    a b g e |
    fis2 fis4 fis |
    
    %page3
    g8 a b g a b c d |
    e4 e, f8 g a b |
    c4 c g8 a b g |
    a4 fis g2 |
    r4 d' c2 |
    
    b a8 b c a |
    b g a4 b e |
    d1
  }
  \alternative { { d2 r4 d } { d1\fermata } }

  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Shoot, false love, I care not,
  Spend thy shafts and spare not.
  
  Fa la la la la la la.
  Fa la __ la la.
  Fa la la la la la la.
  la.
  
  \set stanza = #"1. "
  I fear not I thy might,
  And less I weight thy spite,
  All na -- ked I un -- arm me,
  If thou canst now shoot and harm me,
  So light -- ly I es -- teem thee,
  As now a child I deem thee.
  
  Fa la la la la la la la la la.
  Fa la la la la la.
  Fa la la la la la la la.
  Fa la la la la la la la la la la la la la.
  \set stanza = #"1. "
  I
  la.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Long thy bow did fear me,
  While thy pomp did blear me
 
  "" \repeat unfold 18 \skip1
  
  \set stanza = #"2. "
  But now I do per -- ceive
  Thy art is to de -- ceive,
  And ev -- ’ry sim -- ple lov -- er
  All thy false -- hood can dis -- cov -- er:
  Then weep, love, and be sor -- ry,
  For thou has lost thy glo -- ry.
  
  "" \repeat unfold 37 \skip1
  \set stanza = #"2. "
  But
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c'' {
  \repeat volta 2 {
    g4. g8 g4 fis |
    g2 g |
    g4 g fis g |
    fis2 fis |
    g8. a16 b8 g a4 b |
    
    c2 b4 a~ |
    a g fis g |
    a4. g8 g4 fis |
  } 
  \alternative { { g1 } { g2 r4 b } } 
  \repeat volta 2 {
    b4. c8 c4 b |
    c2 r4 c |
    
    %page2
    b4. b8 a4 g |
    fis1 |
    d'4 d8 d c4 a |
    b2 b4 b8 b |
    a4 g a a |
    
    a2 a4 a |
    c c b b |
    a2 a4 a |
    d d e e |
    a,2 a4 d |
    
    %page3
    b8 c d2 a8 b |
    c d e4 a,2 |
    r2 d |
    c b |
    a r |
    
    r r4 e'~ |
    e d2 c4~ |
    c b4 a2 |
  }
  \alternative { { b2 r4 b } { b1\fermata } }
  \bar "|."
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
quintusMusic = \relative c' {
  \repeat volta 2 {
    e4. d8 e4 d |
    d2 d |
    e4 d d d |
    d2 d |
    d f |
    
    g2. e4 |
    d2 d4 d |
    d4. d8 e4 d |
  }
  \alternative { { d1 } { d2 r4 g } }
  \repeat volta 2 {
    g4. g8 a4 g |
    g2 r4 g |
    
    %page2
    g4. f8 f4 d |
    d1 |
    fis4 fis8 g g4 fis |
    g4 d2 d8 d |
    cis4 d d cis |
    
    d2 d4 a |
    e e' d b |
    cis2 cis4 cis |
    d d d cis |
    d2 d4 d |
    
    %page3
    d4. e8 f2 |
    c4 g' f8 e f g |
    a4 g2 d8 e |
    f4 c d4. e8 |
    f4 f e2 |
    
    d4 d8 e f g a4 |
    g d g a |
    fis4 g2 fis4 |
  }
  \alternative { { g2 r4 g } { g1\fermata} } 
  \bar "|."
}
tenorMusic = \relative c' {
  \repeat volta 2 {
    g4. b8 a4 a |
    g2 g |
    g4 g a b |
    a2 a |
    r4 d a d |
    
    g,4. a8 b4 c~ |
    c8 a b g a4 d |
    a4. b8 a4 a |
  }
  \alternative { { g1 }{ g2 r4 d' } }
  \repeat volta 2 {
    d4. g,8 d'4 d |
    c2 r4 c |
    
    %page2
    d4. d8 a4 b8[ g] |
    a1 |
    d4 d8 b c4 d |
    g,2 g4 g8 g |
    a4 b a a |
    
    d,2 d4 f |
    g a a gis |
    a2 a4 a |
    a g g g |
    a2 a |
    
    %page3
    r4 d2 c4~ |
    c b8 c d4 d |
    e4. c8 d4. b8 |
    c4. a8 b4. g8 |
    a2. e8 f |
    
    g4 d'2 c4 |
    b4 a d8 b c b |
    a4 g a2 |
  }
  \alternative { { g2 r4 d' } { g,1\fermata}}
  \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2 {
    c,4. b8 c4 d |
    g,2 g |
    c4 g d' g |
    d2 d |
    g f4 d |
    
    c8 d e fis g4 a |
    fis4 g d g, |
    fis4. g8 c4 d |
  }
  \alternative { { g,1 } { g2 r4 g' } } 
  \repeat volta 2 {
    g4. e8 f4 g c,2 r4 c |
    
    %page2
    g'4. d8 f4 g |
    d1 |
    r1 |
    r1 |
    r1 |
    
    r2 r4 d4 |
    c a d e |
    a,2 a4 a' |
    fis g e e |
    d2 d |
    
    %page3
    g2 f |
    e d |
    c b |
    a g |
    d'4 d a8 b c4 |
    
    g8 a b c d4 a |
    e' fis g c, |
    d1
  }
  \alternative { { g,2 r4 g' } { g,1\fermata}}
}
bassWords = \lyricmode {
}


altusWordsA = \lyricmode {
  \set stanza = #"1. "
  Shoot, false love, I care not,
  Spend thy shafts and spare not.
  
  Fa la la la la la la.
  Fa la __  la la.
  Fa la la la la la.
  la.
  
  \set stanza = #"1. "
  I fear not I thy might,
  And less I weight thy spite,
  All na -- ked I un -- arm me,
  If thou canst now shoot and harm me,
  So light -- ly I es -- teem thee,
  As now a child I deem thee.
  
  Fa la la la.
  Fa la la la la la.
  Fa la la la.
  Fa __ la la __ la la la.
  \set stanza = #"1. "
  I
  la.
}

altusWordsB = \lyricmode {
  \set stanza = #"2. "
  Long thy bow did fear me,
  While thy pomp did blear me
 
  "" \repeat unfold 17 \skip1
  
  \set stanza = #"2. "
  But now I do per -- ceive
  Thy art is to de -- ceive,
  And ev -- ’ry sim -- ple lov -- er
  All thy false -- hood can dis -- cov -- er:
  Then weep, love, and be sor -- ry,
  For thou has lost thy glo -- ry.
  
  "" \repeat unfold 19 \skip1
  \set stanza = #"2. "
  But
}


quintusWordsA = \lyricmode {
  \set stanza = #"1. "
  Shoot, false love, I care not,
  Spend thy shafts and spare not.
  
  Fa la la.
  Fa la la.
  Fa la la la la la.
  la.
  
  \set stanza = #"1. "
  I fear not I thy might,
  And less I weight thy spite,
  All na -- ked I un -- arm me,
  If thou canst now shoot and harm me,
  So light -- ly I es -- teem thee,
  As now a child I deem thee.
  
  Fa la la la la.
  Fa la la la la la.
  Fa la la la la la la la la la la.
  Fa la la la la la.
  Fa la la la la la la.
  \set stanza = #"1. "
  I
  la.
}

quintusWordsB = \lyricmode {
  \set stanza = #"2. "
  Long thy bow did fear me,
  While thy pomp did blear me
 
  "" \repeat unfold 12 \skip1
  
  \set stanza = #"2. "
  But now I do per -- ceive
  Thy art is to de -- ceive,
  And ev -- ’ry sim -- ple lov -- er
  All thy false -- hood can dis -- cov -- er:
  Then weep, love, and be sor -- ry,
  For thou has lost thy glo -- ry.
  
  "" \repeat unfold 34 \skip1
  \set stanza = #"2. "
  But
}

tenorWordsA = \lyricmode {
  \set stanza = #"1. "
  Shoot, false love, I care not,
  Spend thy shafts and spare not.
  
  Fa la la la la la.
  Fa __ la la la la.
  Fa la la la la la.
  la.
  
  \set stanza = #"1. "
  I fear not I thy might,
  And less I weight thy spite,
  All na -- ked I un -- arm me,
  If thou canst now shoot and harm me,
  So light -- ly I es -- teem thee,
  As now a child I deem thee.
  
  Fa la __ la la la.
  Fa la la la la la la la la la.
  Fa la la.
  Fa la la la la la la la la la la la.
  \set stanza = #"1. "
  I
  la.
}

tenorWordsB = \lyricmode {
  \set stanza = #"2. "
  Long thy bow did fear me,
  While thy pomp did blear me
 
  "" \repeat unfold 17 \skip1
  
  \set stanza = #"2. "
  But now I do per -- ceive
  Thy art is to de -- ceive,
  And ev -- ’ry sim -- ple lov -- er
  All thy false -- hood can dis -- cov -- er:
  Then weep, love, and be sor -- ry,
  For thou has lost thy glo -- ry.
  
  "" \repeat unfold 29 \skip1
  \set stanza = #"2. "
  But
}
bassusWordsA = \lyricmode {
  \set stanza = #"1. "
  Shoot, false love, I care not,
  Spend thy shafts and spare not.
  
  Fa la la la la la la la.
  Fa la la la.
  Fa la la la la la.
  la.
  
  \set stanza = #"1. "
  I fear not I thy might,
  And less I weight thy spite,
  
  So light -- ly I es -- teem thee,
  As now a child I deem thee.
  
  Fa la la la la la la la la.
  Fa la la la la la la la la.
  Fa la la la la la la.
  \set stanza = #"1. "
  I
  la.
}

bassusWordsB = \lyricmode {
  \set stanza = #"2. "
  Long thy bow did fear me,
  While thy pomp did blear me
 
  "" \repeat unfold 18 \skip1
  
  \set stanza = #"2. "
  But now I do per -- ceive
  Thy art is to de -- ceive,
  
  Then weep, love, and be sor -- ry,
  For thou has lost thy glo -- ry.
  
  "" \repeat unfold 24 \skip1
  \set stanza = #"2. "
  But
}


pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altus"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    
   \new Staff = quintus <<
      \clef "treble_8"
      \new Voice = "quintus" { << \global \quintusMusic >> }
    >>
    \new Lyrics = "quintus"  \lyricsto "quintus" \quintusWordsA
    \new Lyrics = "quintusB"  \lyricsto "quintus" \quintusWordsB
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenor"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    
    \new Staff = bassus <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "bassus"  \lyricsto "basses" \bassusWordsA
    \new Lyrics = "bassusB"  \lyricsto "basses" \bassusWordsB
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      %\override VerticalAxisGroup #'staff-affinity = #0
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Shoot false love I care not"}}
  composer = \markup\oldStyleNum"Thomas Morley (1557–1602)"
  tagline = ""
}}




