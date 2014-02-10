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
  first-page-number = #170
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
  \key aes \major
  \time 4/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  %\override TupletBracket #'bracket-visibility = ##f
}

sopMusic = \relative c' {
	f2 \times 2/3 {f4 g aes} g2 f |
  g \times 2/3 {g4 aes bes} aes4.( g8) f2 |
  c' \times 2/3 {bes4 c des} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 |
  
  f2 \times 2/3 {f4 g aes} g2 f |
  g \times 2/3 {g4 aes bes} aes4.( g8) f2 |
  c' \times 2/3 {bes4 c des} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 |
  
  c'2 \times 2/3 {aes4 bes c} bes2 bes |
  aes \times 2/3 {f4 g aes} g2 g |
  f2 \times 2/3 {f4 g aes} bes2 bes |
  aes \times 2/3 {bes4 aes bes} c1 |
  
  f,2 \times 2/3 {f4 g aes} g2 f |
  g \times 2/3 {g4 aes bes} aes4.( g8) f2 |
  c' \times 2/3 {bes4 c des} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Once to _ _ ev -- ’ry man and _ _ na -- tion
  Comes the _ _ mo -- ment to de -- cide,
  In the _ _ strife of truth with _ _ false -- hood,
  For the _ _ good or e -- vil side;
  Some great _ _ cause, some great de -- _ _ ci -- sion,
  Of -- f’ring _ _ each the bloom or _ _ blight,
  And the _ _ choice goes by for -- _ _ ev -- er
  ’Twixt that _ _ dark -- ness and that light.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Then to _ _ side with truth is _ _ no -- ble,
  When we _ _ share her wretch -- ed crust,
  Ere her _ _ cause bring fame and _ _ prof -- it,
  And ’tis _ _ prosp -- ’rous to be just;
  Then it _ _ is the brave man _ _ choos -- es,
  While the _ _ cow -- ard stands a -- _ _ side
  Till the _ _ mul -- ti -- tude make _ _ vir -- tue
  Of the _ _ faith they had de -- nied.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  By the _ _ light of burn -- ing _ _ mar -- tyrs
  Je -- sus’ _ _ bleed -- ing feet I track,
  Toil -- ing _ _ up new Cal -- v’ries _ _ ev -- er
  With the _ _ cross that turns not back;
  New oc -- _ _ ca -- sions teach new _ _ du -- ties,
  Time makes _ _ an -- cient good un -- _ _ couth;
  They must _ _ up -- ward still and _ _ on -- ward
  Who would _ _ keep a -- breast of truth.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Though the _ _ cause of e -- vil _ _ pros -- per,
  Yet ’tis _ _ truth a -- lone is strong;
  Though her _ _ por -- tion be the _ _ scaf -- fold,
  And up -- _ _ on the throne be wrong,
  Yet that _ _ scaf -- fold sways the _ _ fu -- ture,
  And, be -- _ _ hind the dim un -- _ _ known,
  Stand -- eth _ _ God with -- in the _ _ shad -- ow
  Keep -- ing _ _ watch a -- bove his own.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c2 c e f |
  f e f c |
  c \times 2/3 {ees?2 f4} g2 f |
  f e f1 |
  
  c2 c e f |
  f e f c |
  c \times 2/3 {ees?2 f4} g2 f |
  f e f1 |
  
  ees2 ees ees ees |
  c f f e |
  f c f ees |
  c f e1 |
  
  aes,2 f' e f |
  f e f f |
  ees? ees ees f |
  f e f1 \bar"|."
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
  aes2 c c aes |
  des c c4.( bes8) aes2 |
  ees' \times 2/3 {bes2 aes4} c2 c |
  des4.( c8) bes2 aes1 |
  
  aes2 c c aes |
  des c c4.( bes8) aes2 |
  ees' \times 2/3 {bes2 aes4} c2 c |
  des4.( c8) bes2 aes1 |
  
  aes2 aes aes g |
  aes c c c |
  f, aes aes g |
  aes f g1 |
  
  f2 c' c aes |
  des c c4.( bes8) aes2 |
  aes \times 2/3 {g4 aes bes} aes4.( g8) f2 |
  des'4.( c8) bes2 aes1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,2 aes, c des |
  bes c f f |
  aes \times 2/3 {g2 f4} e2 f |
  bes, c f1 |
  
  f,2 aes c des |
  bes c f f |
  aes \times 2/3 {g2 f4} e2 f |
  bes, c f1 |
  
  aes,2 c ees ees |
  f aes, c c |
  f ees des ees |
  f4( ees) des2 c1 |
  
  des2 aes c des |
  bes c f f |
  aes ees aes, des |
  bes c f1 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Once to Every Man and Nation"}}
  poet = \markup\oldStyleNum"James Russell Lowell (1819–1891)"
  composer = \markup\oldStyleNum"Thomas John Williams (1869–1944)"
  tagline = ""
}}
