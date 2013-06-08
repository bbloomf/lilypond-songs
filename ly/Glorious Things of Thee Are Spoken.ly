\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Glorious Things of Thee Are Spoken"}}
  composer = \markup\oldStyleNum"Franz Josef Haydn (1732–1809)"
  tagline = ""
}
\paper {
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4. g8 a4 g |
  bes a g8[ e] f4 |
  d' c bes a |
  g a8[ f] c'2 |
  
  f,4. g8 a4 g |
  bes a g8[ e] f4 |
  d'4 c bes a |
  g a8[ f] c'2\fermata |
  
  g4 a g8[ e] c4 |
  bes' a g8[ e] c4 |
  c' bes a4. a8 |
  b4. b8 c2 |
  
  \repeat volta 2 {
    f4. e8 e[ d] c4 |
    d4. c8 c[ bes] a4 |
    g a8[ bes] c[ d] bes[ g] |
    f4 a8[ g] f2 |
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Glor -- ious things of thee are spok -- en,
  Zi -- on, cit -- y of our God!
  He, Whose word can -- not be brok -- en,
  Formed thee for His own a -- bode;
  On the rock of a -- ges found -- ed,
  What can shake thy sure re -- pose?
  With sal -- va -- tion’s walls sur -- round -- ed
  Thou may’st smile at all thy foes.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  See! the streams of liv -- ing wa -- ters
  Spring -- ing from e -- ter -- nal love,
  Well sup -- ply thy sons and daugh -- ters,
  And all fear of want re -- move:
  Who can faint when such a riv -- er
  Ev -- er flows their thirst t'as -- suage?
  Grace, which like the Lord the giv -- er,
  Nev -- er fails from age to age.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Round each hab -- i -- ta -- tion hov -- ’ring
  See the cloud and fire ap -- pear!
  For a glo -- ry and a cov -- ’ring,
  Show -- ing that the Lord is near:
  Thus de -- riv -- ing from their ban -- ner
  Light by night and shade by day,
  Safe they feed up -- on the man -- na
  Which he gives them when they pray.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Blest in -- hab -- i -- tants of Zi -- on,
  Washed in the Re -- deem -- er’s blood!
  Je -- sus, whom their souls re -- ly on,
  Makes them kings and priests to God;
  ’Tis His love His peo -- ple rais -- es
  O -- ver self to reign as kings,
  And as priests, his sol -- emn prais -- es
  Each for a \once \override LyricHyphen #'minimum-distance = #0.7 thank -- of -- f’ring brings.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Sav -- ior, if of Zi -- on’s cit -- y
  I through grace a mem -- ber am;
  Let the world de -- ride or pit -- y,
  I will glo -- ry in Thy Name;
  Fad -- ing is the world -- ling’s plea -- sure,
  All his boast -- ed pomp and show!
  Sol -- id joys and last -- ing trea -- sure,
  None but Zi -- on’s chil -- dren know.
}

altoMusic = \relative c' {
  f4. f8 f4 e |
  g f e8[ c] c4 |
  d8[ e] f4 e f |
  f f e2 |
  
  f4. f8 f4 e |
  g f e8[ c] c4 |
  d8[ e] f4 e f |
  f f e2 |
  
  e4 f e8[ c] c4 |
  g' f e8[ c] c4 |
  f e f4. f8 |
  f4. f8 e2 |

  \repeat volta 2{
    f4. f8 f4 f |
    f4. f8 e4 f |
    e f8[ g] f4 d |
    c e f2 |
  }
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
  a4. bes8 c4 c |
  c c bes8[ g] a4 |
  bes4 c c c |
  d d c2 |
  
  a4. bes8 c4 c |
  c c bes8[ g] a4 |
  bes4 c c c |
  d d c2 |
  
  c4 c c8[ g] e4 |
  c' c c8[ g] e4 |
  c' c c4. c8 |
  d4. d8 c2 |
  
  \repeat volta 2 {
    c4. c8 bes4 a |
    bes4. a8 g[ c] c4 |
    c c c bes |
    a c8[ bes] a2 |
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4. f8 f4 c |
  e f c f |
  bes a g f |
  bes, b c2 |
  
  f4. f8 f4 c |
  e f c f |
  bes a g f |
  bes, b c2\fermata |
  
  c4 c c c |
  e f c c |
  a' g f4. f8 |
  d4. g8 c,2 |
  
  \repeat volta 2 {
    a'4. a8 bes4 f |
    bes,4. f'8 c4 f |
    c c8[ bes] a4 bes |
    c c f2 |
  }
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
}

\score {
  \unfoldRepeats
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
  \midi {
    \tempo 4 = 100
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}


